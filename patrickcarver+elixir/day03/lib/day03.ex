defmodule Day03 do
  def part1(file_name \\ "example.txt") do
    {numbers, symbols} = parse("priv/" <> file_name, ~r/[^\d\.]/)
    count_part_numbers(numbers, symbols)
  end

  def part2(file_name \\ "example.txt") do
    {numbers, symbols} = parse("priv/" <> file_name, ~r/\*/)

    Enum.reduce(symbols, 0, fn coords, acc ->
      adjacent =
        Enum.filter(numbers, fn {_num, neighbors} -> MapSet.member?(neighbors, coords) end)

      if length(adjacent) == 2 do
        [{first, _}, {second, _}] = adjacent
        acc + first * second
      else
        acc
      end
    end)
  end

  def parse(file_name, regex) do
    stream = from_file(file_name)
    numbers = parse_numbers(stream)
    symbols = parse_symbols(stream, regex)

    {numbers, symbols}
  end

  def from_file(file_name) do
    file_name
    |> File.stream!()
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.with_index()
  end

  def count_part_numbers(numbers, symbols) do
    Enum.reduce(numbers, 0, fn {num, neighbors}, acc ->
      if MapSet.disjoint?(neighbors, symbols) do
        acc
      else
        acc + num
      end
    end)
  end

  def parse_symbols(stream, regex) do
    Stream.map(stream, fn {line, y} ->
      Regex.scan(regex, line, return: :index)
      |> List.flatten()
      |> Enum.map(fn {x, _} -> {x, y} end)
    end)
    |> Enum.to_list()
    |> List.flatten()
    |> MapSet.new()
  end

  def parse_numbers(stream) do
    Enum.map(stream, fn {line, y} ->
      ranges =
        Regex.scan(~r/\d{1,3}/, line, return: :index)
        |> List.flatten()
        |> Enum.map(fn {start, len} -> start..(start + len - 1) end)

      Enum.map(ranges, fn range ->
        number =
          line
          |> String.slice(range)
          |> String.to_integer()

        start..stop = range
        xs = (start - 1)..(stop + 1)
        top = for x <- xs, do: {x, y - 1}
        bottom = for x <- xs, do: {x, y + 1}
        sides = [{start - 1, y}, {stop + 1, y}]

        neighbors = MapSet.new(top ++ bottom ++ sides)

        {number, neighbors}
      end)
    end)
    |> List.flatten()
  end
end
