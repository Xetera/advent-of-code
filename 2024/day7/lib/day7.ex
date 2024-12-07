defmodule Day7 do
  @type expression() :: list(integer())
  @type equation() :: {integer(), expression()}
  @operations1 [:add, :mult]
  @operations2 [:add, :mult, :conc]

  @spec parse(String.t()) :: equation()
  def parse(line) do
    [target | lines] =
      String.split(line, [": ", " "])
      |> Enum.map(fn num -> String.to_integer(num) end)

    {target, lines}
  end

  @spec solve_line(integer(), expression(), list(atom())) :: boolean()
  def solve_line(target, [a], _) when a == target, do: true
  def solve_line(_, [_], _), do: false
  def solve_line(target, [a | _], _) when a > target, do: false

  def solve_line(target, [a, b | rest], ops) do
    Enum.find(ops, fn op ->
      next = calculate(a, b, op)
      solve_line(target, [next | rest], ops)
    end)
  end

  @spec calculate(integer(), integer(), :add | :mult) :: integer()
  def calculate(a, b, :add), do: a + b
  def calculate(a, b, :mult), do: a * b
  def calculate(a, b, :conc), do: a * :math.pow(10, digits(b)) + b

  def solve_with(lines, ops) do
    Enum.chunk_every(lines, 30)
    |> Task.async_stream(fn lines ->
      for {target, nums} <- lines, solve_line(target, nums, ops) do
        target
      end
      |> Enum.sum()
    end)
    |> Enum.reduce(0, fn {:ok, val}, acc -> acc + val end)
  end

  @spec solve(String.t()) :: integer()
  def solve(input) do
    now = :os.system_time(:millisecond)

    result =
      String.split(input, "\n")
      |> Enum.map(&parse/1)
      |> solve_with(@operations1)

    before = :os.system_time(:millisecond)
    IO.puts("Part 1 Took: #{before - now}ms")
    result
  end

  @spec solve_part2(String.t()) :: integer()
  def solve_part2(input) do
    now = :os.system_time(:millisecond)

    result =
      String.split(input, "\n")
      |> Enum.map(&parse/1)
      |> solve_with(@operations2)

    before = :os.system_time(:millisecond)
    IO.puts("Part 2 Took: #{before - now}ms")
    result
  end

  def digits(num) when num >= 100 and num < 1000, do: 3
  def digits(num) when num >= 10 and num < 100, do: 2
  def digits(num) when num >= 0 and num < 10, do: 1
end
