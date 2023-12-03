defmodule Day01 do
  @from_words %{
    "one" => 1,
    "two" => 2,
    "three" => 3,
    "four" => 4,
    "five" => 5,
    "six" => 6,
    "seven" => 7,
    "eight" => 8,
    "nine" => 9
  }

  @nums ~w(1 2 3 4 5 6 7 8 9 one two three four five six seven eight nine ten)

  def part1(file_name \\ "input.txt") do
    "priv/#{file_name}"
    |> stream_from_file()
    |> Enum.reduce(0, fn line, acc ->
      acc + calibration_value(line)
    end)
  end

  def part2(file_name \\ "input.txt") do
    "priv/#{file_name}"
    |> stream_from_file()
    |> Enum.reduce(0, fn line, acc ->
      line
      |> first_and_last()
      |> Kernel.+(acc)
    end)
  end

  def stream_from_file(file_name) do
    file_name
    |> File.stream!()
    |> Stream.map(&String.trim_trailing/1)
  end

  def calibration_value(value) do
    Regex.replace(~r/\D/, value, "")
    |> ensure_two_digits()
    |> String.to_integer()
  end

  def ensure_two_digits(value) do
    case String.length(value) do
      1 -> value <> value
      2 -> value
      _ -> String.at(value, 0) <> String.at(value, -1)
    end
  end

  def first_and_last(value) do
    Enum.reduce(@nums, [], fn n, acc ->
      matches = :binary.matches(value, n)

      case length(matches) do
        0 ->
          acc

        1 ->
          [{index, _len}] = matches
          [{n, index} | acc]

        _ ->
          result = Enum.map(matches, fn {index, _} -> {n, index} end)
          {min, max} = Enum.min_max_by(result, fn {_num, index} -> index end)
          [min, max | acc]
      end
    end)
    |> Enum.min_max_by(fn {_num, index} -> index end)
    |> then(fn {{first, _}, {last, _}} -> to_num(first) * 10 + to_num(last) end)
  end

  def to_num(value) do
    case Integer.parse(value) do
      {num, ""} -> num
      :error -> from_word(value)
    end
  end

  def from_word(word) do
    Map.get(@from_words, word)
  end
end
