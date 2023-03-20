#pragma once

#include "variant_utils.h"

#include <array>
#include <cassert>

template <typename T, typename... Types>
constexpr bool holds_alternative(const variant<Types...>& v) noexcept {
  return detail::index_by_type_v<T, Types...> == v.index();
}

template <std::size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>>& get(variant<Types...>& v) {
  if (I != v.index())
    throw bad_variant_access{};
  return v.storage.template get<I>();
}

template <std::size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>> const& get(variant<Types...> const& v) {
  return get<I>(const_cast<variant<Types...>&>(v));
}

template <std::size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>>&& get(variant<Types...>&& v) {
  return std::move(get<I>(v));
}

template <std::size_t I, typename... Types>
constexpr const variant_alternative_t<I, variant<Types...>>&& get(const variant<Types...>&& v) {
  return std::move(get<I>(v));
}

template <typename T, typename... Types>
constexpr T& get(variant<Types...>& v) {
  return get<detail::index_by_type_v<T, Types...>>(v);
}

template <typename T, typename... Types>
constexpr const T& get(const variant<Types...>& v) {
  return get<detail::index_by_type_v<T, Types...>>(v);
}

template <typename T, typename... Types>
constexpr T&& get(variant<Types...>&& v) {
  return get<detail::index_by_type_v<T, Types...>>(std::forward<variant<Types...>>(v));
}

template <typename T, typename... Types>
constexpr T const&& get(variant<Types...> const&& v) {
  return get<detail::index_by_type_v<T, Types...>>(std::forward<variant<Types...>>(v));
}

template <std::size_t I, typename... Types>
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...>* pv) noexcept {
  if (pv == nullptr || pv->index() != I)
    return nullptr;
  return std::addressof(get<I>(*pv));
}

template <std::size_t I, typename... Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>>
get_if(const variant<Types...>* pv) noexcept {
  return get_if<I>(const_cast<variant<Types...>*>(pv));
}

template <class T, typename... Types>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept {
  return get_if<detail::index_by_type_v<T, Types...>>(pv);
}

template <class T, typename... Types>
constexpr std::add_pointer_t<const T> get_if(const variant<Types...>* pv) noexcept {
  return get_if<detail::index_by_type_v<T, Types...>>(pv);
}

template <std::size_t I, typename Seq>
struct concat;

template <std::size_t I1, std::size_t... I2>
struct concat<I1, std::index_sequence<I2...>> {
  using type = std::index_sequence<I1, I2...>;
};

template <std::size_t N, typename SizeSeq>
struct generate_nth_index_seq;

template <std::size_t N, std::size_t FirstSize, std::size_t... LastSzs>
struct generate_nth_index_seq<N, std::index_sequence<FirstSize, LastSzs...>> {
  using type = typename concat<
      N / ((LastSzs * ...)),
      typename generate_nth_index_seq<N % ((LastSzs * ...)), std::index_sequence<LastSzs...>>::type>::type;
};

template <std::size_t N, std::size_t Size>
struct generate_nth_index_seq<N, std::index_sequence<Size>> {
  using type = std::index_sequence<N>;
};

template <std::size_t... Ixs, typename Visitor, typename... Variants>
constexpr decltype(auto) real_invoker(std::index_sequence<Ixs...>, Visitor&& vis, Variants&&... vars) {
  return std::invoke(std::forward<Visitor>(vis), get<Ixs>(std::forward<Variants>(vars))...);
}

template <typename Seq, typename Visitor, typename... Variants>
constexpr decltype(auto) proxy_invoker(Visitor&& vis, Variants&&... vars) {
  return real_invoker(Seq{}, std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

template <typename Visitor, typename... Variants, std::size_t... Is>
consteval decltype(auto) visit_invoker_generator(std::index_sequence<Is...>) {
  return &proxy_invoker<std::index_sequence<Is...>, Visitor, Variants...>;
}

template <typename Variant>
constexpr std::pair<std::size_t, std::size_t> index_calculator(Variant&& v) {
  return std::make_pair(v.index(), variant_size_v<std::remove_reference_t<Variant>>);
}

template <typename Variant, typename... Variants>
requires(sizeof...(Variants) > 0) constexpr std::pair<std::size_t, std::size_t> index_calculator(Variant&& v,
                                                                                                 Variants&&... vars) {
  auto i = index_calculator(std::forward<Variants>(vars)...);
  return std::make_pair(v.index() * i.second + i.first, i.second * variant_size_v<std::remove_reference_t<Variant>>);
}

template <std::size_t I, std::size_t Sz>
constexpr std::pair<std::size_t, std::size_t> compile_time_index_calc(std::index_sequence<I>, std::index_sequence<Sz>) {
  return std::make_pair(I, Sz);
}

template <std::size_t I, std::size_t... Is, std::size_t Sz, std::size_t... Szs>
requires(sizeof...(Is) >
         0) consteval std::pair<std::size_t, std::size_t> compile_time_index_calc(std::index_sequence<I, Is...>,
                                                                                  std::index_sequence<Sz, Szs...>) {
  auto tailing_res = compile_time_index_calc(std::index_sequence<Is...>{}, std::index_sequence<Szs...>{});
  return std::make_pair(I * tailing_res.second + tailing_res.first, tailing_res.second * Sz);
}

template <std::size_t N, std::size_t... Is>
static constexpr decltype(auto) make_zero_sequence_impl() {
  if constexpr (N == 0)
    return std::integer_sequence<std::size_t, Is...>();
  else
    return make_zero_sequence_impl<N - 1, 0, Is...>();
}

template <std::size_t Sz>
using make_zero_sequence = std::decay_t<decltype(make_zero_sequence_impl<Sz>())>;

template <typename Seq1, typename Seq2>
struct next_sequence;

template <std::size_t First, std::size_t FirstSz>
struct next_sequence<std::index_sequence<First>, std::index_sequence<FirstSz>> {
  using type = std::index_sequence<First + 1>;
};

template <std::size_t First, std::size_t... Ixs, std::size_t FirstSz, std::size_t... Szs>
struct next_sequence<std::index_sequence<First, Ixs...>, std::index_sequence<FirstSz, Szs...>> {
  using type = std::conditional_t<
      First + 1 == FirstSz,
      typename concat<0, typename next_sequence<std::index_sequence<Ixs...>, std::index_sequence<Szs...>>::type>::type,
      std::index_sequence<First + 1, Ixs...>>;
};

template <typename Visitor, typename... Variants, std::size_t... Ixs, std::size_t... Szs>
consteval void put_in_array_by_seq(
    std::index_sequence<Ixs...>, std::index_sequence<Szs...>,
    std::array<decltype(visit_invoker_generator<Visitor, Variants...>(make_zero_sequence<sizeof...(Variants)>{})),
               (variant_size_v<std::remove_reference_t<Variants>> * ...)>& functions) {
  functions[compile_time_index_calc(std::index_sequence<Ixs...>{}, std::index_sequence<Szs...>{}).first] =
      visit_invoker_generator<Visitor, Variants...>(std::index_sequence<Ixs...>{});
  if constexpr (((Ixs + 1 != Szs) || ...)) {
    put_in_array_by_seq<Visitor, Variants...>(
        typename next_sequence<std::index_sequence<Ixs...>, std::index_sequence<Szs...>>::type{},
        std::index_sequence<Szs...>{}, functions);
  }
}

template <typename Visitor, typename... Variants, std::size_t... Ixs>
consteval decltype(auto) fill_arr(std::index_sequence<Ixs...>) {
  std::array<decltype(visit_invoker_generator<Visitor, Variants...>(
                 typename generate_nth_index_seq<
                     0, std::index_sequence<variant_size_v<std::remove_reference_t<Variants>>...>>::type{})),
             (variant_size_v<std::remove_reference_t<Variants>> * ...)>
      functions;

  put_in_array_by_seq<Visitor, Variants...>(make_zero_sequence<sizeof...(Variants)>{},
                                            std::index_sequence<variant_size_v<std::remove_reference_t<Variants>>...>{},
                                            functions);
  return functions;
}

template <typename Visitor, typename... Variants, std::size_t... Ixs>
constexpr decltype(auto) visit_impl(std::index_sequence<Ixs...>, Visitor&& vis, Variants&&... vars) {
  constexpr std::array<decltype(visit_invoker_generator<Visitor, Variants...>(
                           make_zero_sequence<sizeof...(Variants)>{})),
                       (variant_size_v<std::remove_reference_t<Variants>> * ...)>
      functions = fill_arr<Visitor, Variants...>(std::index_sequence<Ixs...>{});

  return functions[index_calculator(std::forward<Variants>(vars)...).first](std::forward<Visitor>(vis),
                                                                            std::forward<Variants>(vars)...);
}

template <typename Visitor, typename... Variants,
          typename Seq = std::make_index_sequence<(variant_size_v<std::remove_reference_t<Variants>> * ...)>>
constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars) {
  if ((std::forward<Variants>(vars).valueless_by_exception() || ...)) {
    throw bad_variant_access{};
  }
  return visit_impl(Seq{}, std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

template <typename... Types>
constexpr bool operator==(const variant<Types...>& v, const variant<Types...>& w) {
  if (v.index() != w.index())
    return false;
  if (v.valueless_by_exception())
    return true;
  return equal_alts(v.index(), v.storage, w.storage);
}

template <typename... Types>
constexpr bool operator!=(const variant<Types...>& v, const variant<Types...>& w) {
  return !(v == w);
}

template <typename... Types>
constexpr bool operator<(const variant<Types...>& v, const variant<Types...>& w) {
  if (w.valueless_by_exception())
    return false;
  if (v.valueless_by_exception())
    return true;
  if (v.index() < w.index())
    return true;
  if (v.index() > w.index())
    return false;
  return less_alts(v.index(), v.storage, w.storage);
}

template <typename... Types>
constexpr bool operator>(const variant<Types...>& v, const variant<Types...>& w) {
  return !(v < w) && (v != w);
}

template <typename... Types>
constexpr bool operator<=(const variant<Types...>& v, const variant<Types...>& w) {
  return !(v > w);
}

template <typename... Types>
constexpr bool operator>=(const variant<Types...>& v, const variant<Types...>& w) {
  return !(v < w);
}

template <typename... Types>
    requires(std::is_move_constructible_v<Types>&&...) &&
    (std::is_swappable_v<Types> && ...) constexpr void swap(variant<Types...>& lhs,
                                                            variant<Types...>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
  lhs.swap(rhs);
}
