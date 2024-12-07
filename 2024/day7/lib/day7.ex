defmodule Day7 do
  @type expression() :: list(integer())
  @type equation() :: {integer(), expression()}
  @operations1 [:add, :mult]
  @operations2 [:add, :mult, :conc]

  @spec parse(String.t()) :: equation()
  def parse(line) do
    [target | lines] =
      String.split(line, [":", " "], trim: true)
      |> Enum.map(fn num ->
        {parsed, ""} = Integer.parse(num, 10)
        parsed
      end)

    {target, lines}
  end

  @spec solve_line(integer(), expression(), list(atom())) :: integer()
  def solve_line(target, [a], _) when a == target, do: 1
  def solve_line(_, [_], _), do: 0
  def solve_line(target, [a | _], _) when a > target, do: 0

  def solve_line(target, [a, b | rest], ops) do
    Enum.map(ops, fn op ->
      next = calculate(a, b, op)
      solve_line(target, [next | rest], ops)
    end)
    |> Enum.sum()
  end

  @spec calculate(integer(), integer(), :add | :mult) :: integer()
  def calculate(a, b, :add), do: a + b
  def calculate(a, b, :mult), do: a * b
  def calculate(a, b, :conc), do: a * :math.pow(10, digits(b)) + b

  def solve_with(lines, ops) do
    for {target, nums} <- lines, solve_line(target, nums, ops) > 0 do
      target
    end
    |> Enum.sum()
  end

  @spec solve(String.t()) :: integer()
  def solve(input) do
    String.split(input, "\n")
    |> Enum.map(&parse/1)
    |> solve_with(@operations1)
  end

  @spec solve(String.t()) :: integer()
  def solve_part2(input) do
    String.split(input, "\n")
    |> Enum.map(&parse/1)
    |> solve_with(@operations2)
  end

  def digits(num) when num >= 0 and num < 10, do: 1
  def digits(num) when num >= 9 and num < 100, do: 2
  def digits(num) when num >= 99 and num < 1000, do: 3
end
