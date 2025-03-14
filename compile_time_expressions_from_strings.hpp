#pragma once

#if __cplusplus < 202302L
    #error out of date c++ version, compile with -stdc++=2c
#elif defined(__clang__) && __clang_major__ < 19
    #error out of date clang, compile with latest version
#elif !defined(__clang__) && defined(__GNUC__) && __GNUC__ < 14
    #error out of date g++, compile with latest version
#elif defined(_MSC_VER)
    #error msvc does not yet support the latest c++ features
#else

#include <algorithm>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <expected> //not needed after changing to c++26's compie time exceptions
#include <functional>
#include <iterator>
#include <optional>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <vector>

// [[c++26]]: change all s_assert's to c++26's compile-time exceptions, then no need for std::expected as well
// add more detailed error messages when c++ has constexpr std::format and compile-time exceptions

namespace cte {
    namespace detail {
        using namespace std::literals;

        //replace with pack indexing on gcc15
        template <std::uintmax_t i, class... tp_types_ts>
        requires (i < sizeof...(tp_types_ts) && sizeof...(tp_types_ts) != 0)
        auto constexpr stogap_pack_index(tp_types_ts&&... p_args) noexcept -> auto&& {
            return std::get<i>(std::forward_as_tuple(std::forward<tp_types_ts>(p_args)...));
        }

        template <auto&& tp_outer_ref>
        auto constexpr function_or_variable_identifier = []<auto&& tp_inner_ref> {
            auto constexpr static s_function_signature = __PRETTY_FUNCTION__;
            auto constexpr static s_data = std::string_view(s_function_signature);
            auto constexpr static s_begin =
                #if defined(_MSC_VER)
                    std::ranges::next(std::ranges::find_subrange(s_data, " __cdecl "sv ), 7)
                #else
                    std::ranges::next(std::ranges::find(s_data, '='), 2)
                #endif
            ;
            auto constexpr static s_end =
                #if defined(_MSC_VER)
                    std::ranges::find(s_data, '(')
                #else
                    std::ranges::prev(std::ranges::end(s_data))
                #endif
            ;
            auto l_result = std::array<char, std::ranges::distance(s_begin, s_end)>{};
            std::ranges::copy(s_begin, s_end, std::ranges::begin(l_result));
            return l_result;
        }.template operator()<tp_outer_ref>();

        template <std::size_t tp_size>
        struct string {
            std::array<char, tp_size> m_data;
            consteval string(const char (&p_data)[tp_size]) : m_data{std::to_array(p_data)} {}
            consteval string(const std::array<char, tp_size>& p_data) : m_data{p_data} {}
        };
        template <std::size_t tp_size>
        string(const char (&m_data)[tp_size]) -> string<tp_size>;
        template <std::size_t tp_size>
        string(const std::array<char, tp_size>&) -> string<tp_size>;

        auto constexpr and_fn                    = [](auto&&... p_predicates) { return [...c_predicates = p_predicates](auto&& p_argument) { return (... && c_predicates(c_predicates, p_argument)); }; };
        auto constexpr or_fn                     = [](auto&&... p_predicates) { return [...c_predicates = p_predicates](auto&& p_argument) { return (... || c_predicates(c_predicates, p_argument)); }; };
        auto constexpr any_of                    = [](auto&& p_value, auto&&... p_values) { return (... || (p_value == p_values)); };
        auto constexpr all_of                    = [](auto&& p_value, auto&&... p_values) { return (... && (p_value == p_values)); };
        auto constexpr any_of_range              = [](auto&& p_value, auto&&... p_values) { return (... || (std::ranges::equal(p_value, p_values))); };
        auto constexpr is_whitespace             = [](auto&& p_char_comparable) { return any_of(p_char_comparable, ' ', '\t', '\n', '\v', '\f', '\r', '\0'); }; //probably shouldn't treat null termination as whitespace here, however then would need to conditionally remove the trailing null termination from input data
        auto constexpr is_digit                  = [](auto&& p_char_comparable) { return p_char_comparable >= '0' && p_char_comparable <= '9'; };
        auto constexpr is_alpha                  = [](auto&& p_char_comparable) { return (p_char_comparable >= 'a' && p_char_comparable <= 'z') || (p_char_comparable >= 'A' && p_char_comparable <= 'Z'); };
        auto constexpr is_alnum                  = [](auto&& p_char_comparable) { return is_digit(p_char_comparable) || is_alpha(p_char_comparable); };
        auto constexpr is_alpha_underscore       = [](auto&& p_char_comparable) { return is_alpha(p_char_comparable) || p_char_comparable == '_'; };
        auto constexpr is_alnum_underscore       = [](auto&& p_char_comparable) { return is_alnum(p_char_comparable) || p_char_comparable == '_'; };
        auto constexpr is_symbol                 = [](auto&& p_char_comparable) { return !is_alnum_underscore(p_char_comparable); };
        auto constexpr is_parenthesis            = [](auto&& p_char_comparable) { return p_char_comparable == '(' || p_char_comparable == ')'; };
        auto constexpr is_valid_operator         = [](auto&& p_char_comparable) { return any_of(p_char_comparable, '+', '-', '*', '/', '%', '^', ',', '(', ')'); };

        template <auto tp_opening_delimiter, auto tp_closing_delimiter> //*not proper find_unenclosed*
        auto constexpr find_unenclosed = [](auto p_first, auto p_last, auto&& p_value) {
            auto l_depth = std::size_t{0};
            return std::ranges::subrange{std::ranges::find_if(p_first, p_last, [&](auto a) mutable {
                return (a == tp_opening_delimiter ? ++l_depth : a == tp_closing_delimiter && l_depth != 0 ? --l_depth : l_depth) == 0 && a == p_value;
            }), p_last};
        };

        using const_char_subrange_t = std::ranges::subrange<std::add_pointer_t<std::add_const_t<char>>>;

        struct token {
            const_char_subrange_t m_value;
            bool                  m_is_special;
            auto constexpr get_precedence() const -> std::uint8_t { return std::ranges::equal(m_value, "^"sv) ? 0 : any_of_range(m_value, "+"sv, "-"sv) ? (m_is_special ? 3 : 1) : 2; }
            auto constexpr is(auto... p_chars)             const -> bool { return (... || (m_value.front() == p_chars)) && std::ranges::size(m_value) == 1; }
            auto constexpr is_not(auto... p_chars)         const -> bool { return !is(p_chars...); }
            auto constexpr is_operator()                   const -> bool { return is_symbol(m_value.front()) && m_value.front() != '\"'; }
            auto constexpr is_identifier()                 const -> bool { return is_alpha_underscore(m_value.front()); }
            auto constexpr is_string_literal()             const -> bool { return m_value.front()== '\"'; }
            auto constexpr is_integral_literal()           const -> bool { return is_digit(m_value.front()); }
            auto constexpr operator==(const char p_char)   const -> bool { return is(p_char); }
            auto constexpr operator==(auto&& p_char_range) const -> bool { return std::ranges::equal(m_value, p_char_range); }
        };
        template <std::size_t tp_size>
        struct identifier { std::array<char, tp_size> m_data; };

        struct invalid_expression_t {};
        auto constexpr invalid_expression = invalid_expression_t{};
        struct function_call_end_t {};
        auto constexpr function_call_end = function_call_end_t{};

        template <std::array tp_input, auto&&... tp_functions_and_variables>
        decltype(auto) constexpr evaluate_expression_impl = [] -> decltype(auto) {
            if constexpr (std::ranges::empty(tp_input) || (std::ranges::size(tp_input) == 1 && tp_input.back() == '\0')) {
                static_assert(false, "input data was empty");
                return invalid_expression;
            }
            else if constexpr (!(... && [] {
                if constexpr (std::ranges::count_if(std::array{std::string_view{function_or_variable_identifier<tp_functions_and_variables>}...}, [](auto&& a) {
                    return std::ranges::equal(a, function_or_variable_identifier<tp_functions_and_variables>);
                }) != 1) {
                    static_assert(false, "duplicate argument '"s + std::ranges::to<std::string>(function_or_variable_identifier<tp_functions_and_variables>) + "' detected"); //change to constexpr std::format when available);
                    return false;
                }
                else return true;
            }())) {
                return invalid_expression;
            }
            else {
                auto constexpr static s_expected_tokens = [] {
                    auto s_tokenize = [] {
                        auto l_result = std::vector<token>{};
                        for (auto l_subrange = std::ranges::subrange{tp_input}; !std::ranges::empty(l_subrange);) {
                            auto l_sentinel =
                                is_digit(l_subrange.front())      ? std::ranges::find_if(l_subrange, std::not_fn(is_digit)) :
                                is_alpha(l_subrange.front())      ? std::ranges::find_if(l_subrange, std::not_fn(is_alnum_underscore)) :
                                is_whitespace(l_subrange.front()) ? std::ranges::find_if(l_subrange, std::not_fn(is_whitespace)) :
                                std::ranges::next(std::ranges::begin(l_subrange));
                            if (l_subrange.front() == '\"')
                                if (auto l_it = std::ranges::adjacent_find(l_subrange, [](auto a, auto b) { return a != '\\' && b == '\"'; }))
                                    l_sentinel = l_it == std::ranges::end(l_subrange) ? l_it : std::ranges::next(l_it, 2);
                            if (!is_whitespace(l_subrange.front()))
                                l_result.emplace_back(const_char_subrange_t{std::ranges::begin(l_subrange), l_sentinel}, false);
                            l_subrange.advance(std::ranges::distance(std::ranges::begin(l_subrange), l_sentinel));
                        }
                        for (auto l_token = std::ranges::begin(l_result); l_token != std::ranges::end(l_result); ++l_token) {
                            if (l_token->is('(') && l_token != std::ranges::begin(l_result) && !std::ranges::prev(l_token)->is_operator()) {
                                l_token->m_is_special = true;
                                if (auto l_sr = find_unenclosed<'(', ')'>(l_token, std::ranges::end(l_result), ')'))
                                    std::ranges::begin(l_sr)->m_is_special = true;
                            }
                            else if (l_token->is_not(')'))
                                l_token->m_is_special =
                                    (l_token->is('(') && l_token != std::ranges::begin(l_result) && !std::ranges::prev(l_token)->is_operator()) ||
                                    (any_of(*l_token, '+' , '-') && (l_token == std::ranges::begin(l_result) || (std::ranges::prev(l_token)->is_operator() && std::ranges::prev(l_token)->is_not(')'))));
                        }
                        return l_result;
                    };
                    auto l_tokens = std::array<token, std::ranges::size(s_tokenize())>{};
                    using return_type = std::expected<decltype(l_tokens), std::string_view>;
                    std::ranges::copy(s_tokenize(), std::ranges::begin(l_tokens));
                    if (std::ranges::empty(l_tokens))
                        return return_type{std::unexpect, "expression is empty/no tokens lexed."};
                    if (std::ranges::any_of(l_tokens, [](auto a) { return is_symbol(a.m_value.front()) && !is_valid_operator(a) && a.m_value.front() != '\"'; }))
                        return return_type{std::unexpect, "invalid operator token"};
                    auto l_opening_parenthesis_count = std::ranges::count_if(l_tokens, [](auto a) { return a.is('('); });
                    auto l_closing_parenthesis_count = std::ranges::count_if(l_tokens, [](auto a) { return a.is(')'); });
                    if (l_opening_parenthesis_count > l_closing_parenthesis_count)
                        return return_type{std::unexpect, "expected ')'"};
                    if (l_opening_parenthesis_count < l_closing_parenthesis_count)
                        return return_type{std::unexpect, "expected '('"};
                    if (std::ranges::count_if(std::views::adjacent<2>(l_tokens), [](auto a) { return std::get<0>(a) != '\\' && std::get<1>(a) == '\"'; }) % 2 != 0)
                        return return_type{std::unexpect, "expected '\"'"};
                    if (
                        (l_tokens.front().is_operator() && !l_tokens.front().m_is_special && l_tokens.front().is_not('(')) ||
                        (l_tokens.back().is_operator() && l_tokens.back().is_not(')')) ||
                        (std::ranges::adjacent_find(l_tokens, [](auto a, auto b) {
                            return
                            (all_of(',', a, b)) ||
                            (a.is_operator() && a.is_not('(', ')') && b.is(',', ')') ) ||
                            (a.is(',', '(') && b.is_operator() && b.is_not('(') && !b.m_is_special) ||
                            ([](auto... p_ab) { return (... && (!is_parenthesis(p_ab.m_value.front()) && p_ab.is_operator() && (!p_ab.m_is_special))); }(a, b));
                        }) != std::ranges::end(l_tokens)))
                            return return_type{std::unexpect, "expected an expression"};
                    if (std::ranges::adjacent_find(l_tokens, [](auto... p_ab) { return (... && (!p_ab.is_operator())); }) != std::ranges::end(l_tokens))
                        return return_type{std::unexpect, "expected end after expression"};
                    if (std::ranges::empty(l_tokens))
                        return return_type{std::unexpect, "expression is empty/no tokens lexed."};
                    if (std::ranges::empty(l_tokens))
                        return return_type{std::unexpect, "expression is empty/no tokens lexed."};
                    return return_type{std::move(l_tokens)};
                }();
                if constexpr (!s_expected_tokens.has_value()) {
                    static_assert(false, s_expected_tokens.error());
                    return invalid_expression;
                }
                else {
                    auto constexpr static s_tokens = s_expected_tokens.value();
                    //assert all comma operators are parenthesized
                    auto constexpr static s_tokens_arranged = [] {
                        auto l_operator_stack = std::vector<token>{};
                        auto l_output_stack   = std::vector<token>{};
                        for (auto l_token = std::ranges::begin(s_tokens); l_token != std::ranges::end(s_tokens); ++l_token) {
                            if (l_token->is(',')) {
                                auto l_view = l_operator_stack | std::views::reverse | std::views::take_while([](auto a) { return a.is_not('('); });
                                std::ranges::move(l_view, std::back_inserter(l_output_stack));
                                l_operator_stack.resize(static_cast<std::ranges::range_size_t<decltype(l_operator_stack)>>(std::ranges::ssize(l_operator_stack) - std::ranges::distance(l_view)));
                                l_output_stack.emplace_back(std::move(*l_token));
                            }
                            else if (l_token->is(')')) {
                                auto l_enclosed_operators = std::ranges::find_last_if(l_operator_stack, [](auto&& a) { return a.is('('); });
                                std::ranges::move(l_enclosed_operators | std::views::drop(1) | std::views::reverse, std::back_inserter(l_output_stack));
                                if (l_enclosed_operators.front().m_is_special)
                                    l_output_stack.emplace_back(std::move(*l_token));
                                l_operator_stack.resize(static_cast<std::ranges::range_size_t<decltype(l_operator_stack)>>(std::ranges::ssize(l_operator_stack) - std::ranges::distance(l_enclosed_operators)));
                            }
                            else if (l_token->is('(')) {
                                l_operator_stack.emplace_back(*l_token);
                                if (l_token->m_is_special)
                                    l_output_stack.emplace_back(std::move(*l_token));
                            }
                            else if (l_token->is_operator()) {
                                if (std::ranges::empty(l_operator_stack) || l_operator_stack.back().is('(') || l_token->get_precedence() > l_operator_stack.back().get_precedence())
                                    l_operator_stack.emplace_back(std::move(*l_token));
                                else {
                                    auto l_view = l_operator_stack | std::views::reverse | std::views::take_while([&](auto a) { return a.get_precedence() >= l_token->get_precedence(); });
                                    std::ranges::move(l_view, std::back_inserter(l_output_stack));
                                    l_operator_stack.resize(static_cast<std::ranges::range_size_t<decltype(l_operator_stack)>>(std::ranges::ssize(l_operator_stack) - std::ranges::distance(l_view)));
                                    l_operator_stack.emplace_back(std::move(*l_token));
                                }
                            }
                            else l_output_stack.emplace_back(std::move(*l_token));
                        }
                        std::ranges::move(std::views::reverse(l_operator_stack), std::back_inserter(l_output_stack));
                        auto l_result = std::array<token, std::ranges::size(s_tokens) - std::ranges::count_if(s_tokens, [](auto&& a) { return is_parenthesis(a) && !a.m_is_special; })>{};
                        std::ranges::move(l_output_stack, std::ranges::begin(l_result));
                        return l_result;
                    }();
                    return []<std::size_t tp_index, auto&&... tp_values>(this auto p_self) -> decltype(auto) {
                        [[maybe_unused]] auto constexpr static s_outer = p_self;
                        auto constexpr static s_value_count = sizeof...(tp_values);
                        if constexpr (tp_index < std::ranges::size(s_tokens_arranged)) {
                            auto constexpr static s_current_token = s_tokens_arranged[tp_index];
                            if constexpr (s_current_token.is(',')) {
                                static_assert(s_value_count == 1, "expected a ';'");
                                return std::tuple{tp_index + 1, std::forward_as_tuple(stogap_pack_index<s_value_count - 1>(tp_values...)), std::ignore};
                            }
                            else if constexpr (s_current_token.is(')')) {
                                static_assert(s_value_count <= 1, "expected a ';'");
                                if constexpr (s_value_count == 0)
                                    return std::tuple{tp_index + 1, std::tuple{}, function_call_end};
                                else return std::tuple{tp_index + 1, std::forward_as_tuple(stogap_pack_index<s_value_count - 1>(tp_values...)), function_call_end};
                            }
                            else if constexpr (s_current_token.is('(')) {
                                auto constexpr static && s_callee = stogap_pack_index<s_value_count - 1>(tp_values...);
                                //switch to c++26's constexpr static structured binding:
                                // auto constexpr static [s_next_index, s_function_invocation_result] = the lambda below;
                                auto constexpr static s_function_invocation_result = []<std::size_t tp_jndex, auto&&... tp_arguments>(this auto p_self) -> decltype(auto) {
                                    //switch to c++26's constexpr static structured binding with a placeholder:
                                    // auto constexpr static [s_next_index, s_optional_argument, _] = s_outer.template operator()<tp_jndex>();
                                    auto constexpr static s_result            = s_outer.template operator()<tp_jndex>(); //use constexpr structured bindings here
                                    auto constexpr static s_next_index        = std::get<0>(s_result); //not needed after above
                                    auto constexpr static s_optional_argument = std::get<1>(s_result); //not needed after above
                                    if constexpr (std::same_as<function_call_end_t, std::remove_const_t<std::tuple_element_t<2, decltype(s_result)>>>) {
                                        decltype(auto) constexpr static s_invocation_return_value = std::apply([](auto&&... p_optional_argument) -> decltype(auto) {
                                            static_assert(requires { s_callee(tp_arguments..., p_optional_argument...); }, "no matching call to function '"s + std::ranges::to<std::string>(function_or_variable_identifier<s_callee>) + '\'');
                                            static_assert(!requires { { s_callee(tp_arguments..., p_optional_argument...) } -> std::same_as<void>; }, "function '"s + std::ranges::to<std::string>(function_or_variable_identifier<s_callee>) + "' must return a value/return type must not be of type 'void'");
                                            return s_callee(tp_arguments..., p_optional_argument...);
                                        }, s_optional_argument);
                                        return std::pair<decltype(s_next_index), decltype(s_invocation_return_value)>{s_next_index, s_invocation_return_value};
                                    }
                                    else return p_self.template operator()<s_next_index, tp_arguments..., std::get<0>(s_optional_argument)>();
                                }.template operator()<tp_index + 1>();
                                if constexpr (s_function_invocation_result.first == std::ranges::size(s_tokens_arranged))
                                    return s_function_invocation_result.second;
                                else return []<std::size_t... tp_is>(std::index_sequence<tp_is...>) -> decltype(auto) {
                                    return s_outer.template operator()<s_function_invocation_result.first, stogap_pack_index<tp_is>(tp_values...)..., s_function_invocation_result.second>();
                                }(std::make_index_sequence<s_value_count - 1>{});
                            }
                            else if constexpr (s_current_token.is_operator()) {
                                return []<std::size_t... tp_is>(std::index_sequence<tp_is...>, auto&& p_outer) {
                                    auto constexpr static s_invocation_return_value = [] {
                                        static_assert(s_value_count >= (s_current_token.m_is_special ? 1 : 2), "expected an expression");
                                        if constexpr (std::ranges::equal(s_current_token.m_value, "+"sv)) {
                                            if constexpr (s_current_token.m_is_special)
                                                static_assert(requires { +stogap_pack_index<s_value_count - 1>(tp_values...); }, "no matching call to operator+");
                                            else static_assert(requires { stogap_pack_index<s_value_count - 2>(tp_values...) + stogap_pack_index<s_value_count - 1>(tp_values...); }, "no matching call to operator+");
                                            if constexpr (s_current_token.m_is_special)
                                                return +stogap_pack_index<s_value_count - 1>(tp_values...);
                                            else return stogap_pack_index<s_value_count - 2>(tp_values...) + stogap_pack_index<s_value_count - 1>(tp_values...);
                                        }
                                        else if constexpr (std::ranges::equal(s_current_token.m_value, "-"sv)) {
                                            if constexpr (s_current_token.m_is_special)
                                                static_assert(requires { -stogap_pack_index<s_value_count - 1>(tp_values...); }, "no matching call to operator-");
                                            else static_assert(requires { stogap_pack_index<s_value_count - 2>(tp_values...) - stogap_pack_index<s_value_count - 1>(tp_values...); }, "no matching call to operator-");
                                            if constexpr (s_current_token.m_is_special)
                                                return -stogap_pack_index<s_value_count - 1>(tp_values...);
                                            else return stogap_pack_index<s_value_count - 2>(tp_values...) - stogap_pack_index<s_value_count - 1>(tp_values...);
                                        }
                                        else if constexpr (std::ranges::equal(s_current_token.m_value, "*"sv)) {
                                            if constexpr (s_current_token.m_is_special)
                                                static_assert(requires { *stogap_pack_index<s_value_count - 1>(tp_values...); }, "no matching call to operator*");
                                            else static_assert(requires { stogap_pack_index<s_value_count - 2>(tp_values...) * stogap_pack_index<s_value_count - 1>(tp_values...); }, "no matching call to operator*");
                                            if constexpr (s_current_token.m_is_special)
                                                return *stogap_pack_index<s_value_count - 1>(tp_values...);
                                            else return stogap_pack_index<s_value_count - 2>(tp_values...) * stogap_pack_index<s_value_count - 1>(tp_values...);
                                        }
                                        else if constexpr (std::ranges::equal(s_current_token.m_value, "/"sv)) {
                                            static_assert(requires { stogap_pack_index<s_value_count - 2>(tp_values...) / stogap_pack_index<s_value_count - 1>(tp_values...); }, "no matching call to operator/");
                                            return stogap_pack_index<s_value_count - 2>(tp_values...) / stogap_pack_index<s_value_count - 1>(tp_values...);
                                        }
                                        else if constexpr (std::ranges::equal(s_current_token.m_value, "%"sv)) {
                                            static_assert(requires { stogap_pack_index<s_value_count - 2>(tp_values...) % stogap_pack_index<s_value_count - 1>(tp_values...); }, "no matching call to operator%");
                                            return stogap_pack_index<s_value_count - 2>(tp_values...) % stogap_pack_index<s_value_count - 1>(tp_values...);
                                        }
                                        else if constexpr (std::ranges::equal(s_current_token.m_value, "^"sv)) {
                                            static_assert(requires { stogap_pack_index<s_value_count - 2>(tp_values...) ^ stogap_pack_index<s_value_count - 1>(tp_values...); }, "no matching call to operator^");
                                            return stogap_pack_index<s_value_count - 2>(tp_values...) ^ stogap_pack_index<s_value_count - 1>(tp_values...);
                                        }
                                    }();
                                    if constexpr (tp_index == std::ranges::size(s_tokens_arranged) - 1)
                                        return s_invocation_return_value;
                                    else return p_outer.template operator()<tp_index + 1, stogap_pack_index<tp_is>(tp_values...)..., s_invocation_return_value>();
                                }(std::make_index_sequence<s_value_count - (s_current_token.m_is_special ? 1 : 2)>{}, p_self);
                            }
                            else {
                                if constexpr (is_digit(s_current_token.m_value.front())) {
                                    auto constexpr static s_make_value = [] {
                                        auto l_value = std::uintmax_t{0};
                                        auto l_result = std::from_chars(std::ranges::begin(s_current_token.m_value), std::ranges::end(s_current_token.m_value), l_value);
                                        return l_result.ec == std::errc::result_out_of_range ? std::optional<std::intmax_t>{} : std::optional<std::intmax_t>{l_value};
                                    };
                                    if constexpr (auto constexpr static s_result = s_make_value(); s_result.has_value()) {
                                        auto constexpr static s_value = s_result.value();
                                        return p_self.template operator()<tp_index + 1, tp_values..., s_value>();
                                    }
                                    else static_assert(false, "integral literal was to large for std::intmax_t");
                                }
                                else if constexpr (s_current_token.m_value.front() == '\"') {
                                    auto constexpr static s_value = std::string_view{std::ranges::begin(s_current_token.m_value), std::ranges::prev(std::ranges::end(s_current_token.m_value))};
                                    return p_self.template operator()<tp_index + 1, tp_values..., s_value>();
                                }
                                else {
                                    auto constexpr static && s_function_or_varible_reference = []<std::size_t tp_jndex>(this auto p_self) -> auto&& {
                                        if constexpr (tp_jndex < sizeof...(tp_functions_and_variables)) {
                                            if constexpr (std::ranges::equal(s_current_token.m_value, function_or_variable_identifier<stogap_pack_index<tp_jndex>(tp_functions_and_variables...)>))
                                                return stogap_pack_index<tp_jndex>(tp_functions_and_variables...);
                                            else return p_self.template operator()<tp_jndex + 1>();
                                        }
                                        else static_assert(false, "undeclared identifier '"s + std::ranges::to<std::string>(s_current_token.m_value) + "\'");
                                    }.template operator()<0>();
                                    return p_self.template operator()<tp_index + 1, tp_values..., s_function_or_varible_reference>();
                                }
                            }
                        }
                        else {
                            static_assert(s_value_count == 1, "expected a ';'");
                        }
                    }.template operator()<0>();
                }
            }
        }();
    }
    template <detail::string tp_input, auto&&... tp_functions_and_variables>
    requires (!requires { { detail::evaluate_expression_impl<tp_input.m_data, tp_functions_and_variables...> } -> std::same_as<detail::invalid_expression_t>; })
    decltype(auto) constexpr evaluate_expression = detail::evaluate_expression_impl<tp_input.m_data, tp_functions_and_variables...>;
}
#endif
