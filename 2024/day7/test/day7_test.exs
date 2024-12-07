defmodule Day7Test do
  use ExUnit.Case, async: true
  doctest Day7

  test "part1-test" do
    test_contents = File.read!("input-test.txt")
    assert Day7.solve(test_contents) == 3749
  end

  test "part1" do
    test_contents = File.read!("input.txt")
    assert Day7.solve(test_contents) == 1_298_103_531_759
  end

  test "part2-test" do
    test_contents = File.read!("input-test.txt")
    assert Day7.solve_part2(test_contents) == 11387
  end

  test "part2" do
    test_contents = File.read!("input.txt")
    assert Day7.solve_part2(test_contents) == 140_575_048_428_831
  end

  test "digits" do
    assert Day7.digits(500) == 3
  end

  test "concat" do
    assert Day7.calculate(12, 345, :conc) == 12345
    assert Day7.calculate(123, 45, :conc) == 12345
  end
end
