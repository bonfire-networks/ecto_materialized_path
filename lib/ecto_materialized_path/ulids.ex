defmodule EctoMaterializedPath.UIDs do
  @behaviour Ecto.Type

  @moduledoc """
  Right now it's implemented the same as { :array, Needle.UID }
  """

  def cast(list) when is_list(list) do
    path_is_correct? = Enum.all?(list, fn(path_id) ->
        with {:ok, _} <- Needle.UID.cast(path_id) do
            true
          else _ ->
            false
        end
      end)

    if path_is_correct? do
      { :ok, list }
    else
      :error
    end
  end
  def cast(_), do: :error

  def dump(value), do: { :ok, Enum.map(value, fn path_id -> with {:ok, dumped} <- Needle.UID.dump(path_id) do dumped end end) }

  def load(value), do: { :ok, Enum.map(value, fn path_id -> with {:ok, loaded} <- Needle.UID.load(path_id) do loaded end end) }

  def equal?(one, two), do: one == two

  def type, do: EctoMaterializedPath.UIDs
end
