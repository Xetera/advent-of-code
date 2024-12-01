#include <vector>
#include <fstream>
#include <format>
#include <string>
#include <format>

using std::string;

std::vector<string> inputFor(const std::string& name) {
  std::ifstream file(std::format("./inputs/{}.txt", name));
  std::vector<string> lines;
  string line;
  while (std::getline(file, line)) {
      lines.push_back(line);
  }
  return lines;
}
