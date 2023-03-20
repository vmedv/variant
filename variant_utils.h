#pragma once

template <typename... Types>
struct variant;

struct bad_variant_access : public std::exception {
  bad_variant_access() noexcept = default;
  bad_variant_access(const bad_variant_access& other) noexcept = default;
  const char* what() const noexcept override {
    return "access to the wrong alternative";
  }
};

template <class T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};

template <class T>
inline constexpr in_place_type_t<T> in_place_type{};

template <std::size_t I>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};

template <std::size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

namespace detail {

template <typename... Types>
struct union_wrapper;

template <typename... Types>
struct storage_t;

template <std::size_t I, typename... Types>
struct type_by_index;

template <std::size_t I, typename Head, typename... Tail>
struct type_by_index<I, Head, Tail...> {
  using type = std::conditional_t<I == 0, Head, typename type_by_index<I - 1, Tail...>::type>;
};

template <std::size_t I, typename Type>
struct type_by_index<I, Type> {
  using type = std::conditional_t<I == 0, Type, bad_variant_access>;
};

template <std::size_t I, typename... Types>
using type_by_index_t = typename type_by_index<I, Types...>::type;

template <typename T, typename... Types>
struct index_by_type;

template <typename T, typename... Types>
struct index_by_type<T, T, Types...> {
  static constexpr inline std::size_t value = 0;
};

template <typename T, typename Head, typename... Tail>
struct index_by_type<T, Head, Tail...> {
  static constexpr inline std::size_t value = 1 + index_by_type<T, Tail...>::value;
};

template <typename T, typename... Types>
constexpr inline std::size_t index_by_type_v = index_by_type<T, Types...>::value;

template <typename T, typename U>
concept valid_converting_overload_impl = requires(U && value) {
  T{std::forward<U>(value)};
};

template <typename T, typename U>
concept valid_converting_overload = valid_converting_overload_impl<T[], U>;

template <typename T>
struct is_in_place_index : std::false_type {};

template <std::size_t I>
struct is_in_place_index<in_place_index_t<I>> : std::true_type {};

template <typename T>
struct is_in_place_type : std::false_type {};

template <typename T>
struct is_in_place_type<in_place_type_t<T>> : std::true_type {};

template <typename T, typename U>
struct overload_function {
  constexpr static T resolution(T t) requires valid_converting_overload<T, U>;
};

template <typename U, typename... Types>
struct overload_resolution : overload_function<Types, U>... {
  using overload_function<Types, U>::resolution...;
};

template <typename Overload, typename T>
concept callable_resolution = requires {
  {Overload::resolution(std::declval<T>())};
};

template <typename U, typename... Types>
requires callable_resolution<overload_resolution<U, Types...>, U> using overload_resolution_type =
    decltype(overload_resolution<U, Types...>::resolution(std::declval<U>()));

template <typename T, typename... Types>
concept valid_converting = requires(T && t) {
  (!std::same_as<std::remove_cvref_t<T>, variant<Types...>>)&&(!is_in_place_index<T>::value) &&
      (!is_in_place_type<T>::value);
  {overload_resolution<T, Types...>::resolution(std::forward<T>(t))};
};

template <typename... Types>
concept copy_constructible = std::conjunction_v<std::is_copy_constructible<Types>...>;

template <typename... Types>
concept trivially_copy_constructible =
    std::conjunction_v<std::is_trivially_copy_constructible<Types>...> && copy_constructible<Types...>;

template <typename... Types>
concept copy_assignable = (std::is_copy_constructible_v<Types> && ...) && (std::is_copy_assignable_v<Types> && ...) &&
                          (std::is_destructible_v<Types> && ...);

template <typename... Types>
concept trivially_copy_assignable =
    (std::is_trivially_copy_constructible_v<Types> && ...) && (std::is_trivially_copy_assignable_v<Types> && ...) &&
    (std::is_trivially_destructible_v<Types> && ...) && copy_assignable<Types...>;

template <typename... Types>
concept move_constructible = std::conjunction_v<std::is_move_constructible<Types>...>;

template <typename... Types>
concept trivially_move_constructible =
    std::conjunction_v<std::is_trivially_move_constructible<Types>...> && move_constructible<Types...>;

template <typename... Types>
concept move_assignable = (std::is_move_constructible_v<Types> && ...) && (std::is_move_assignable_v<Types> && ...) &&
                          (std::is_destructible_v<Types> && ...);

template <typename... Types>
concept trivially_move_assignable =
    (std::is_trivially_move_constructible_v<Types> && ...) && (std::is_trivially_move_assignable_v<Types> && ...) &&
    (std::is_trivially_destructible_v<Types> && ...) && move_assignable<Types...>;

} // namespace detail

struct valueless_by_exception_t {};

struct monostate {};

constexpr bool operator==(monostate, monostate) noexcept {
  return true;
}
constexpr bool operator!=(monostate, monostate) noexcept {
  return false;
}
constexpr bool operator<(monostate, monostate) noexcept {
  return false;
}
constexpr bool operator>(monostate, monostate) noexcept {
  return false;
}
constexpr bool operator<=(monostate, monostate) noexcept {
  return true;
}
constexpr bool operator>=(monostate, monostate) noexcept {
  return true;
}

inline constexpr std::size_t variant_npos = -1;

// variant size
template <typename T>
struct variant_size;

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename T>
struct variant_size<const T> {
  static constexpr inline std::size_t value = variant_size<T>::value;
};

template <typename T>
struct variant_size<volatile T> {
  static constexpr inline std::size_t value = variant_size<T>::value;
};

template <typename T>
struct variant_size<const volatile T> {
  static constexpr inline std::size_t value = variant_size<T>::value;
};

template <class T>
inline constexpr std::size_t variant_size_v = variant_size<T>::value;

template <std::size_t I, typename T>
struct variant_alternative;

template <std::size_t I, typename... Types>
struct variant_alternative<I, variant<Types...>> {
  using type = typename detail::type_by_index<I, Types...>::type;
};

template <std::size_t I, typename T>
struct variant_alternative<I, const T> {
  using type = std::add_const_t<typename variant_alternative<I, T>::type>;
};

template <std::size_t I, typename T>
struct variant_alternative<I, volatile T> {
  using type = std::add_volatile_t<typename variant_alternative<I, T>::type>;
};

template <std::size_t I, typename T>
struct variant_alternative<I, const volatile T> {
  using type = std::add_cv_t<typename variant_alternative<I, T>::type>;
};

template <std::size_t I, class T>
using variant_alternative_t = typename variant_alternative<I, T>::type;
