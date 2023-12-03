defmodule Day02 do
  @default_colors %{"red" => 0, "green" => 0, "blue" => 0}

  def part1(file_name \\ "example.txt") do
    file_name
    |> from_file()
    |> parse_games()
    |> sum_possible_game_ids(%{"red" => 12, "green" => 13, "blue" => 14})
  end

  def part2(file_name \\ "example.txt") do
    file_name
    |> from_file()
    |> parse_games()
    |> game_power()
  end

  def game_power(games) do
    Enum.reduce(games, 0, fn %{id: _id, sets: sets}, acc ->
      Enum.reduce(sets, @default_colors, fn set, current ->
        Map.merge(current, set, fn _k, v1, v2 -> Enum.max([v1, v2]) end)
      end)
      |> Map.values()
      |> Enum.product()
      |> Kernel.+(acc)
    end)
  end

  def sum_possible_game_ids(games, total_cubes) do
    Enum.reduce(games, 0, fn %{id: id, sets: sets}, acc ->
      are_all_sets_possible =
        Enum.map(sets, fn set ->
          Map.merge(total_cubes, set, fn _k, v1, v2 -> v1 - v2 end)
          |> Enum.all?(fn {_k, v} -> v >= 0 end)
        end)
        |> Enum.all?()

      if are_all_sets_possible do
        acc + id
      else
        acc
      end
    end)
  end

  def parse_games(lines) do
    lines
    |> Stream.map(fn "Game " <> rest ->
      [game_id, set_text] = String.split(rest, ": ")

      sets =
        set_text
        |> String.split("; ")
        |> Enum.map(fn text ->
          text
          |> String.split(", ")
          |> Enum.reduce(@default_colors, fn cube_text, acc ->
            [num_text, color] = String.split(cube_text)
            Map.put(acc, color, String.to_integer(num_text))
          end)
        end)

      %{id: String.to_integer(game_id), sets: sets}
    end)
  end

  def from_file(file_name) do
    ("priv/" <> file_name)
    |> File.stream!()
    |> Stream.map(&String.trim_trailing/1)
  end
end
