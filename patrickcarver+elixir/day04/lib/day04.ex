defmodule Day04 do
  def part1(file_name \\ "example.txt") do
    ("priv/" <> file_name)
    |> from_file()
    |> parse_for_points()
    |> total_points()
  end

  def part2(file_name \\ "example.txt") do
    ("priv/" <> file_name)
    |> from_file()
    |> parse_for_cards()
    |> add_cards()
    |> total_cards()
  end

  def total_cards(cards) do
    Enum.reduce(cards, 0, fn {_id, %{copies: copies}}, acc ->
      acc + copies
    end)
  end

  def add_cards(cards, current_id \\ 1) do
    card = Map.get(cards, current_id)
    next_current_id = current_id + 1
    next_card = Map.get(cards, next_current_id)

    if next_card == nil do
      cards
    else
      %{matches: matches, copies: copies} = card

      if matches == 0 do
        add_cards(cards, next_current_id)
      else
        range = next_current_id..(next_current_id + matches - 1)

        new_cards =
          Enum.reduce(range, cards, fn id, acc ->
            {_, new_acc} = get_and_update_in(acc[id].copies, &{&1, &1 + copies})
            new_acc
          end)

        add_cards(new_cards, next_current_id)
      end
    end
  end

  def total_points(stream) do
    Enum.reduce(stream, 0, fn [winning, possess], acc ->
      num_matches = MapSet.intersection(winning, possess) |> Enum.count()

      if num_matches == 0 do
        acc
      else
        acc + 2 ** (num_matches - 1)
      end
    end)
  end

  def total_matches(winning_text, possess_text) do
    winning = MapSet.new(winning_text)
    possess = MapSet.new(possess_text)

    MapSet.intersection(winning, possess) |> Enum.count()
  end

  def parse_for_cards(stream) do
    Enum.reduce(stream, %{}, fn line, acc ->
      [[id_text], winning_text, possess_text] =
        line
        |> String.replace("Card", "")
        |> String.split(~w[: |])
        |> Enum.map(&String.split/1)

      id = String.to_integer(id_text)
      matches = total_matches(winning_text, possess_text)

      Map.put(acc, id, %{matches: matches, copies: 1})
    end)
  end

  def parse_for_points(stream) do
    Stream.map(stream, fn line ->
      line
      |> String.split(~w[: |])
      |> Enum.drop(1)
      |> Enum.map(fn text ->
        text
        |> String.split()
        |> MapSet.new()
      end)
    end)
  end

  def from_file(file_name) do
    file_name
    |> File.stream!()
    |> Stream.map(&String.trim_trailing/1)
  end
end
