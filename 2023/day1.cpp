#include "helpers.hpp"
#include <ranges>
#include <iostream>
#include <optional>

std::pair<int, int> parseInput(const std::string& line) {
  int start = -1;
  int end = -1;
  for (size_t i = 0; i < line.size(); ++i) {
    const auto num = line[i] - '0';
    if (num >= 0 && num <= 9) {
      start = num;
      break;
    }
  }
  for (size_t i = line.size(); i > 0; --i) {
    const auto num = line[i] - '0';
    if (num >= 0 && num <= 9) {
      end = num;
      break;
    }
  }
  return std::pair<int, int>(start, end);
}

int main() {
  const auto lines = inputFor("day1");
  int sum = 0;
  for (const auto& [start, end] : lines | std::views::transform(parseInput)) {
    sum += start + end;
  }
  std::cout << sum << std::endl;
  return 0;
}
