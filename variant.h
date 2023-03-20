#pragma once

#include <concepts>
#include <exception>
#include <functional>
#include <memory>

#include "variant_non_member_functions.h"
#include "variant_utils.h"

template <typename... Types>
struct variant;

namespace detail {

template <typename... Types>
struct union_wrapper_destructor;

template <typename Head, typename... Tail>
struct union_wrapper_destructor<Head, Tail...> {
  std::size_t index_{0};
  union {
    Head head;
    union_wrapper<Tail...> tail;
  };

  constexpr union_wrapper_destructor() noexcept(
      std::is_nothrow_default_constructible_v<Head>) requires std::is_default_constructible_v<Head> : head() {}

  constexpr union_wrapper_destructor() noexcept(std::is_nothrow_default_constructible_v<Head>) : tail() {}

  constexpr void destroy(std::size_t index) {
    if (index == 0) {
      head.~Head();
    } else {
      tail.destroy(index - 1);
    }
  }

  constexpr ~union_wrapper_destructor() noexcept {
    destroy(index_);
  }
};

template <typename Head, typename... Tail>
    requires std::is_trivially_destructible_v<Head> &&
    (std::is_trivially_destructible_v<Tail> && ...) struct union_wrapper_destructor<Head, Tail...> {
  std::size_t index_{0};
  union {
    Head head;
    union_wrapper<Tail...> tail;
  };

  constexpr union_wrapper_destructor() noexcept(
      std::is_nothrow_default_constructible_v<Head>) requires std::is_default_constructible_v<Head> : head() {}

  constexpr union_wrapper_destructor() noexcept(std::is_nothrow_default_constructible_v<Head>) : tail() {}

  constexpr void destroy(std::size_t) noexcept {}

  constexpr ~union_wrapper_destructor() noexcept = default;
};

template <typename Head, typename... Tail>
struct union_wrapper<Head, Tail...> : union_wrapper_destructor<Head, Tail...> {
  using base = union_wrapper_destructor<Head, Tail...>;

  constexpr union_wrapper() noexcept : base() {}

  constexpr union_wrapper(union_wrapper const&) requires trivially_copy_constructible<Head, Tail...> = default;

  constexpr union_wrapper(union_wrapper&&) requires trivially_move_constructible<Head, Tail...> = default;

  constexpr union_wrapper& operator=(union_wrapper const&) requires trivially_copy_assignable<Head, Tail...> = default;

  constexpr union_wrapper& operator=(union_wrapper&&) requires trivially_move_assignable<Head, Tail...> = default;

  template <std::size_t I>
  constexpr variant_alternative_t<I, variant<Head, Tail...>>& get() {
    if constexpr (I == 0) {
      return base::head;
    } else {
      return base::tail.template get<I - 1>();
    }
  }

  template <std::size_t I, typename... Args>
  constexpr void put(Args&&... args) {
    if constexpr (I == 0) {
      std::construct_at(std::addressof(base::head), std::forward<Args>(args)...);
    } else {
      std::construct_at(std::addressof(base::tail));
      base::tail.template put<I - 1>(std::forward<Args>(args)...);
    }
  }

  template <typename T, typename... Args>
  constexpr void emplace(in_place_type_t<T>, Args&&... args) {
    constexpr std::size_t I = index_by_type_v<T, Head, Tail...>;
    emplace(in_place_index<I>, std::forward<Args>(args)...);
  }

  template <std::size_t I, typename... Args>
  constexpr void emplace(in_place_index_t<I>, Args&&... args) {
    put<I>(std::forward<Args>(args)...);
  }

  constexpr void copy_content(std::size_t index, union_wrapper<Head, Tail...> const& other) {
    if (index == 0) {
      std::construct_at(std::addressof(base::head), other.head);
    } else {
      base::tail.copy_content(index - 1, other.tail);
    }
  }

  constexpr void move_content(std::size_t index, union_wrapper<Head, Tail...>&& other) {
    if (index == 0) {
      std::construct_at(std::addressof(base::head), std::move(other.head));
    } else {
      base::tail.move_content(index - 1, std::move(other.tail));
    }
  }

  constexpr void copy_assign_content(std::size_t index, union_wrapper<Head, Tail...> const& rhs) {
    if (index == 0) {
      base::head = rhs.head;
    } else {
      base::tail.copy_assign_content(index - 1, rhs.tail);
    }
  }

  constexpr void move_assign_content(std::size_t index, union_wrapper<Head, Tail...>&& rhs) {
    if (index == 0) {
      base::head = std::move(rhs.head);
    } else {
      base::tail.move_assign_content(index - 1, std::move(rhs.tail));
    }
  }

  template <std::size_t I, typename T>
  constexpr void assign_content(T&& t) {
    if constexpr (I == 0) {
      base::head = std::forward<T>(t);
    } else {
      base::tail.template assign_content<I - 1>(std::forward<T>(t));
    }
  }

  constexpr void swap(std::size_t index, union_wrapper<Head, Tail...>& rhs) {
    if (index == 0) {
      using std::swap;
      swap(base::head, rhs.head);
    } else {
      base::tail.swap(index - 1, rhs.tail);
    }
  }

  friend constexpr bool equal_alts(std::size_t i, union_wrapper const& a, union_wrapper const& b) {
    if (i == 0)
      return a.head == b.head;
    else {
      return equal_alts(i - 1, a, b);
    }
  }

  friend constexpr bool less_alts(std::size_t i, union_wrapper const& a, union_wrapper const& b) {
    if (i == 0)
      return a.head < b.head;
    else {
      return less_alts(i - 1, a, b);
    }
  }
};

template <>
struct union_wrapper<valueless_by_exception_t> {
  constexpr void copy_content(std::size_t, union_wrapper<valueless_by_exception_t> const&) {}
  constexpr void move_content(std::size_t, union_wrapper<valueless_by_exception_t>&&) {}
  constexpr void copy_assign_content(std::size_t, union_wrapper<valueless_by_exception_t> const&) {}
  constexpr void move_assign_content(std::size_t, union_wrapper<valueless_by_exception_t>&&) {}
  constexpr void assign_content(std::size_t, union_wrapper<valueless_by_exception_t>&&) {}
  constexpr void swap(std::size_t, union_wrapper<valueless_by_exception_t>&) {}
  constexpr void destroy(std::size_t) noexcept {}

  template <std::size_t I>
  constexpr void emplace(in_place_index_t<I>) {}
  template <std::size_t I, typename... Args>
  constexpr void put(Args&&...) {}

  template <std::size_t I>
  constexpr valueless_by_exception_t get() const {
    return valueless_by_exception_t{};
  }
};

} // namespace detail

template <typename... Types>
struct variant {
public:
  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<detail::type_by_index_t<0, Types...>>)
      requires std::is_default_constructible_v<detail::type_by_index_t<0, Types...>> = default;

  constexpr ~variant() noexcept = default;

  constexpr variant(variant const& other) requires detail::trivially_copy_constructible<Types...> = default;

  constexpr variant(variant const& other) requires detail::copy_constructible<Types...> : storage() {
    storage.copy_content(other.index(), other.storage);
    storage.index_ = other.index();
  }

  constexpr variant(variant&& other) noexcept(std::conjunction_v<std::is_nothrow_move_constructible<Types>...>)
      requires detail::trivially_move_constructible<Types...> = default;

  constexpr variant(variant&& other) noexcept(std::conjunction_v<std::is_nothrow_move_constructible<Types>...>)
      requires detail::move_constructible<Types...> : storage() {
    safe_move_content(other.index(), std::forward<storage_t>(other.storage));
  }

  template <typename T>
  constexpr variant(T&& t) noexcept(
      std::is_nothrow_constructible_v<typename detail::overload_resolution_type<T, Types...>, T>)
      requires(detail::valid_converting<T, Types...>&&
                   std::is_constructible_v<typename detail::overload_resolution_type<T, Types...>, T>)
      : variant(
            in_place_index<detail::index_by_type_v<typename detail::overload_resolution_type<T, Types...>, Types...>>,
            std::forward<T>(t)) {}

  template <typename T, typename... Args>
      constexpr explicit variant(in_place_type_t<T>, Args&&... args) requires(std::same_as<T, Types> || ...) &&
      (std::constructible_from<T, Args...>)
      : variant(in_place_index<detail::index_by_type_v<T, Types...>>, std::forward<Args>(args)...) {}

  template <std::size_t I, typename... Args>
  constexpr explicit variant(in_place_index_t<I>, Args&&... args)
      requires((I < sizeof...(Types)) && (std::constructible_from<detail::type_by_index_t<I, Types...>, Args...>))
      : storage() {
    storage.template emplace(in_place_index<I>, std::forward<Args>(args)...);
    storage.index_ = I;
  }

  constexpr variant& operator=(variant const& rhs) requires detail::trivially_copy_assignable<Types...> = default;

  constexpr variant& operator=(variant const& rhs) requires detail::copy_assignable<Types...> {
    if (storage.index_ != rhs.storage.index_) {
      if (visit(
              []<typename T>(T) -> bool {
                return std::is_nothrow_copy_constructible_v<T> || !std::is_nothrow_move_constructible_v<T>;
              },
              rhs)) {
        destroy(storage.index_);
        storage.copy_content(rhs.storage.index_, rhs.storage);
        storage.index_ = rhs.storage.index_;
      } else {
        this->operator=(variant(rhs));
      }
    } else {
      if (index() != variant_npos) {
        storage.copy_assign_content(storage.index_, rhs.storage);
      }
    }
    return *this;
  }

  constexpr variant& operator=(variant&& rhs) noexcept(
      ((std::is_nothrow_move_constructible_v<Types> &&
        std::is_nothrow_move_assignable_v<Types>)&&...)) requires detail::trivially_move_assignable<Types...> = default;

  constexpr variant& operator=(variant&& rhs) noexcept(
      ((std::is_nothrow_move_constructible_v<Types> &&
        std::is_nothrow_move_assignable_v<Types>)&&...)) requires detail::move_assignable<Types...> {
    if (storage.index_ != rhs.storage.index_) {
      storage.destroy(storage.index_);
      safe_move_content(rhs.storage.index_, std::forward<storage_t>(rhs.storage));
    } else {
      if (index() != variant_npos) {
        storage.move_assign_content(storage.index_, std::move(rhs.storage));
      }
    }
    return *this;
  }

  template <typename T>
  constexpr variant& operator=(T&& t) noexcept(
      std::is_nothrow_assignable_v<typename detail::overload_resolution_type<T, Types...>&, T>&&
          std::is_nothrow_constructible_v<typename detail::overload_resolution_type<T, Types...>, T>)
      requires(detail::valid_converting<T, Types...>&&
                   std::is_assignable_v<typename detail::overload_resolution_type<T, Types...>&, T>&&
                       std::is_constructible_v<typename detail::overload_resolution_type<T, Types...>, T>) {
    using type = typename detail::overload_resolution_type<T, Types...>;
    constexpr std::size_t dst_index = detail::index_by_type_v<type, Types...>;
    if (dst_index == index()) {
      storage.template assign_content<dst_index>(std::forward<T>(t));
    } else {
      if (std::is_nothrow_constructible_v<type, T> || !std::is_nothrow_move_constructible_v<type>) {
        emplace<dst_index>(std::forward<T>(t));
      } else {
        emplace<dst_index>(type(std::forward<T>(t)));
      }
    }
    storage.index_ = dst_index;
    return *this;
  }

  constexpr std::size_t index() const noexcept {
    return storage.index_;
  }

  template <typename T, typename... Args>
  constexpr T& emplace(Args&&... args) requires std::is_constructible_v<T, Args...> {
    return emplace<detail::index_by_type_v<T, Types...>>(std::forward<Args>(args)...);
  }

  template <std::size_t I, typename... Args>
  constexpr variant_alternative_t<I, variant>&
  emplace(Args&&... args) requires std::is_constructible_v<detail::type_by_index_t<I, Types...>, Args...> {
    destroy(storage.index_);
    storage.emplace(in_place_index<I>, std::forward<Args>(args)...);
    storage.index_ = I;
    return get<I>(*this);
  }

  constexpr void swap(variant& rhs) {
    if (index() == rhs.index()) {
      storage.swap(storage.index_, rhs.storage);
    } else {
      variant tmp(std::move(*this));
      *this = std::move(rhs);
      rhs = std::move(tmp);
    }
  }

  constexpr bool valueless_by_exception() const noexcept {
    return index() == variant_npos;
  };

private:
  using storage_t = detail::union_wrapper<Types..., valueless_by_exception_t>;
  storage_t storage;

  constexpr void safe_move_content(std::size_t dst_index, storage_t&& other) {
    try {
      storage.move_content(dst_index, std::move(other));
    } catch (...) {
      set_to_valueless();
      throw;
    }
    storage.index_ = dst_index;
  }

  constexpr void set_to_valueless() {
    storage.index_ = variant_npos;
    storage.emplace(in_place_index<sizeof...(Types)>);
  }

  constexpr void destroy(std::size_t index) {
    storage.destroy(index);
    set_to_valueless();
  }

  template <std::size_t I, typename... Tps>
  friend constexpr variant_alternative_t<I, variant<Tps...>>& get(variant<Tps...>& v);

  template <std::size_t I, typename... Tps>
  friend constexpr variant_alternative_t<I, variant<Tps...>> const& get(variant<Tps...> const& v);

  template <std::size_t I, typename... Tps>
  friend constexpr variant_alternative_t<I, variant<Tps...>>&& get(variant<Tps...>&& v);

  template <std::size_t I, typename... Tps>
  friend constexpr const variant_alternative_t<I, variant<Tps...>>&& get(const variant<Tps...>&& v);

  template <typename... Tps>
  friend constexpr bool operator==(const variant<Tps...>& v, const variant<Tps...>& w);

  template <typename... Tps>
  friend constexpr bool operator<(const variant<Tps...>& v, const variant<Tps...>& w);
};
