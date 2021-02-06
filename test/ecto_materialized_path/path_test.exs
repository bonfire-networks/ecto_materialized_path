defmodule EctoMaterializedPath.IntegersTest do
  use ExUnit.Case, async: true

  alias EctoMaterializedPath.Integers

  test "passes with empty array" do
    assert Integers.cast([]) == { :ok, [] }
  end

  test "passes with correct path" do
    assert Integers.cast([13, 45, 18]) == { :ok, [13, 45, 18] }
  end

  test "fails with random value" do
    assert Integers.cast(4) == :error
  end

  test "fails with wrongs path" do
    assert Integers.cast([14, "ee", 45]) == :error
  end
end
