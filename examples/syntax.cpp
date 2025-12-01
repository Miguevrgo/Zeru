#include <algorithm>
#include <array>
#include <charconv>
#include <fstream>
#include <optional>
#include <print>
#include <sstream>
#include <string>
#include <vector>

struct Vector3 {
  float x, y, z;
};

Vector3 create_vector(float x, float y) { return {x, y, 0.0f}; }

int main() {
  const auto pos = create_vector(10.5f, 20.0f);
  const std::array<int, 3> numbers = {1, 2, 3};
  std::vector<uint32_t> dyn_numbers;
  std::vector<int> dyn_numbers_explicit;

  int count = 100;

  if (pos.x < 50.0f) {
    std::print("Position is small: {}\n", pos.x);
  } else if (pos.x > 100.0f) {
    std::println("Position is big: {}", pos.x * 2.0f);
  } else {
    std::println(stderr, "Position should not be inside [50-100]");
  }

  while (count > 0) {
    count = count - 1;
    count -= std::clamp(static_cast<float>(count), 0.0f, 200.0f);
  }

  std::ifstream t("input.txt");
  std::stringstream buffer;
  buffer << t.rdbuf();
  const std::string file_content = buffer.str();

  std::ofstream output_file("output.txt");

  // std::string::split is not available in std (requires std::views::split +
  // ranges conversions) Using manual split for equivalent logic
  std::string line;
  std::stringstream ss(file_content);

  while (std::getline(ss, line, '\n')) {
    if (line.contains('=')) {
      // split_once not available in std
      size_t delim_pos = line.find('=');
      std::string part1 = line.substr(delim_pos + 1);

      std::optional<int> key;
      int temp_val;
      auto [ptr, ec] =
          std::from_chars(part1.data(), part1.data() + part1.size(), temp_val);
      if (ec == std::errc()) {
        key = temp_val;
      } else {
        key = std::nullopt;
      }

      if (key != std::nullopt) {
        std::print(output_file, "{}", key.value());
      }
    }
  }
}
