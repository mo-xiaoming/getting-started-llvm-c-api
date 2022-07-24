#ifndef PL0_UTILS_STATIC_MAP_HPP__
#define PL0_UTILS_STATIC_MAP_HPP__

#include <algorithm>
#include <array>
#include <optional>

namespace utils::container {
namespace internal {
template <typename K, typename V, auto N>
concept suitable_for_small_map = sizeof(std::pair<K, V>) <= 32 && N <= 32;
}

template <typename K, typename V, auto N>
requires internal::suitable_for_small_map<K, V, N>
struct static_map_t {
  // NOLINTNEXTLINE(misc-non-private-member-variables-in-classes)
  std::array<std::pair<K, V>, N> m_data;

  using key_type = K;
  using mapped_type = V;
  using value_type = typename decltype(m_data)::value_type;
  using size_type = std::decay_t<decltype(N)>;

  [[nodiscard]] consteval size_type size() const noexcept { return N; }
  template <typename U> [[nodiscard]] constexpr std::optional<mapped_type> find(const U& key) const noexcept {
    auto const it = std::find_if(m_data.cbegin(), m_data.cend(), [&key](auto const& p) { return p.first == key; });
    if (it != m_data.cend()) {
      return it->second;
    }
    return std::nullopt;
  }
  template <typename U> [[nodiscard]] constexpr bool contains(const U& key) const noexcept {
    return std::any_of(m_data.cbegin(), m_data.cend(), [&key](auto const& p) { return p.first == key; });
  }
};

template <typename K, typename V, auto N> constexpr auto make_static_map(std::array<std::pair<K, V>, N> data) {
  return utils::container::static_map_t<K, V, data.size()>{{data}};
}
} // namespace utils::container
#endif
