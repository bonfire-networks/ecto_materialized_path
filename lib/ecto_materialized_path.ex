defmodule EctoMaterializedPath do
  use Untangle 

  defmacro __using__(opts) do

    column_name = Keyword.get(opts, :column_name, "path")
    namespace = Keyword.get(opts, :namespace, nil)
    method_namespace = if is_nil(namespace), do: nil, else: "#{namespace}_"

    quote bind_quoted: [
      column_name: column_name,
      method_namespace: method_namespace,
    ] do

      ~w(
        parent
        parent_id
        root
        root?
        root_id
        ancestors
        ancestor_ids
        path_ids
        path
        depth
      ) |> Enum.each(fn(function_name) ->
        def unquote(:"#{method_namespace}#{function_name}")(schema = %{ __struct__: __MODULE__ }) do
          path = Map.get(schema, unquote(:"#{column_name}"), [])
          apply(EctoMaterializedPath, unquote(:"#{function_name}"), [schema, path])
        end
      end)

      def unquote(:"#{method_namespace}children")(schema = %{ __struct__: __MODULE__ }) do
        EctoMaterializedPath.children(schema, unquote(:"#{column_name}"))
      end

      def unquote(:"#{method_namespace}siblings")(schema = %{ __struct__: __MODULE__ }) do
        EctoMaterializedPath.siblings(schema, unquote(:"#{column_name}"))
      end

      def unquote(:"#{method_namespace}descendants")(schema = %{ __struct__: __MODULE__ }) do
        EctoMaterializedPath.descendants(schema, unquote(:"#{column_name}"))
      end

      def unquote(:"#{method_namespace}subtree")(schema = %{ __struct__: __MODULE__ }) do
        EctoMaterializedPath.subtree(schema, unquote(:"#{column_name}"))
      end

      def unquote(:"#{method_namespace}build_child")(schema = %{ __struct__: __MODULE__ }) do
        EctoMaterializedPath.build_child(schema, unquote(:"#{column_name}"))
      end

      def unquote(:"#{method_namespace}make_child_of")(changeset = %Ecto.Changeset{ data: %{ __struct__: __MODULE__ } }, parent = %{ __struct__: __MODULE }) do
        EctoMaterializedPath.make_child_of(changeset, parent, unquote(:"#{column_name}"))
      end
      def unquote(:"#{method_namespace}make_child_of")(schema = %{ __struct__: __MODULE__ }, parent = %{ __struct__: __MODULE__ }) do
        EctoMaterializedPath.make_child_of(Ecto.Changeset.change(schema, %{}), parent, unquote(:"#{column_name}"))
      end

      def unquote(:"#{method_namespace}where_depth")(query \\ __MODULE__, depth_params) 
      def unquote(:"#{method_namespace}where_depth")(query = %Ecto.Query{ from: { _, __MODULE__ } }, depth_params) do
        EctoMaterializedPath.where_depth(query, depth_params, unquote(:"#{column_name}"))
      end
      def unquote(:"#{method_namespace}where_depth")(query = %Ecto.Query{}, depth_params) do
        EctoMaterializedPath.where_depth(query, depth_params, unquote(:"#{column_name}"))
      end
      def unquote(:"#{method_namespace}where_depth")(schema = %{ __struct__: __MODULE__ }, depth_params) do
        EctoMaterializedPath.where_depth(schema, depth_params, unquote(:"#{column_name}"))
      end
      def unquote(:"#{method_namespace}where_depth")(schema = __MODULE__, depth_params) do
        EctoMaterializedPath.where_depth(schema, depth_params, unquote(:"#{column_name}"))
      end 

      def unquote(:"#{method_namespace}arrange")(structs_list) when is_list(structs_list), do: EctoMaterializedPath.arrange(structs_list, unquote(:"#{column_name}"))

    end
  end

  require Ecto.Query
  require Logger

  def parent(schema = %{ __struct__: struct, }, path) do
    parent_id = parent_id(schema, path)
    Ecto.Query.from(q in struct, where: q.id in ^parent_id, limit: 1)
  end

  def parent_id(_, path), do: List.last(path)

  def root(schema = %{ __struct__: struct }, path) when is_list(path) do
    root_id = root_id(schema, path)
    Ecto.Query.from(q in struct, where: q.id in ^root_id, limit: 1)
  end

  def root_id(%{ id: id }, []) when is_integer(id) or is_binary(id), do: id
  def root_id(_, path) when is_list(path), do: path |> List.first()

  def root?(%{ id: id }, []) when is_integer(id) or is_binary(id), do: true
  def root?(_, path) when is_list(path), do: false

  def ancestors(schema = %{ __struct__: struct }, path) when is_list(path) do
    Ecto.Query.from(q in struct, where: q.id in ^ancestor_ids(schema, path))
  end

  def ancestor_ids(_, path) when is_list(path), do: path

  def path_ids(struct = %{ id: id }, path), do: ancestor_ids(struct, path) ++ [id]

  def path(struct = %{ __struct__: module }, path) do
    path_ids = path_ids(struct, path)
    Ecto.Query.from(q in module, where: q.id in ^path_ids)
  end

  def children(schema = %{ __struct__: module, id: id }, column_name) do
    path = Map.get(schema, column_name, []) ++ [id]
    Ecto.Query.from(q in module, where: fragment("(?) = ?", field(q, ^column_name), ^path))
  end

  def siblings(schema = %{ __struct__: module }, column_name) do
    path = Map.get(schema, column_name, [])
    Ecto.Query.from(q in module, where: fragment("? = ?", field(q, ^column_name), ^path))
  end

  def descendants(schema = %{ __struct__: module, id: id }, column_name) do
    path = Map.get(schema, column_name, []) ++ [id]
    Ecto.Query.from(q in module, where: fragment("? @> ?", field(q, ^column_name), ^path))
  end

  def subtree(schema = %{ __struct__: module, id: id }, column_name) do
    path = Map.get(schema, column_name, []) ++ [id]
    Ecto.Query.from(q in module, where: fragment("? @> ?", field(q, ^column_name), ^path) or q.id == ^id)
  end

  def depth(_, path) when is_list(path), do: length(path)

  def where_depth(query = %Ecto.Query{}, depth_options, column_name) when is_list(depth_options) do
    do_where_depth(query, depth_options, column_name)
  end
  def where_depth(module, depth_options, column_name) when is_list(depth_options) do
    Ecto.Query.from(q in module)
    |> do_where_depth(depth_options, column_name)
  end

  defp do_where_depth(query, [is_bigger_than: ibt], column_name) when is_integer(ibt) and ibt > 0 do
    Ecto.Query.from(q in query, where: fragment("CARDINALITY(?) > ?", field(q, ^column_name), ^ibt))
  end
  defp do_where_depth(query, [is_bigger_than_or_equal_to: ibtoet], column_name) when is_integer(ibtoet) and ibtoet >= 0 do
    Ecto.Query.from(q in query, where: fragment("CARDINALITY(?) >= ?", field(q, ^column_name), ^ibtoet))
  end
  defp do_where_depth(query, [is_equal_to: iet], column_name) when is_integer(iet) and iet > 0 do
    Ecto.Query.from(q in query, where: fragment("CARDINALITY(?) = ?", field(q, ^column_name), ^iet))
  end
  defp do_where_depth(query, [is_smaller_than_or_equal_to: istoet], column_name) when is_integer(istoet) and istoet >= 0 do
    Ecto.Query.from(q in query, where: fragment("CARDINALITY(?) <= ?", field(q, ^column_name), ^istoet))
  end
  defp do_where_depth(query, [is_smaller_than: ist], column_name) when is_integer(ist) and ist > 0 do
    Ecto.Query.from(q in query, where: fragment("CARDINALITY(?) < ?", field(q, ^column_name), ^ist))
  end
  defp do_where_depth(_, _, _) do
    raise ArgumentError, "invalid arguments"
  end

  def build_child(schema = %{ __struct__: struct, id: id }, column_name) when (is_integer(id) or is_binary(id)) and is_atom(column_name) do
    new_path = Map.get(schema, column_name, []) ++ [id]

    %{ __struct__: struct } |> Map.put(column_name, new_path)
  end

  def make_child_of(changeset, parent = %{ id: id }, column_name) do
    new_path = Map.get(parent, column_name, []) ++ [id]

    changeset |> Ecto.Changeset.change(%{ :"#{column_name}" => new_path })
  end

  def arrange([], _), do: []
  def arrange(nodes_list, column_name) do
    arrange_nodes(nodes_list, column_name)
    |> Map.get(:tree)
  end

  def arrange_nodes([], _), do: %{
      max_depth: 0,
      node_count: 0,
      tree: []
    }
  def arrange_nodes(nodes_list, column_name) do
    nodes_depth_map = nodes_list |> nodes_by_depth_map(%{}, column_name)
    |> debug("nodes_depth_map")

    nodes_depth_keys = nodes_depth_map |> Map.keys() 
    |> debug("nodes_depth_keys")

    max_depth_level = nodes_depth_keys |> Enum.max()
    |> debug("max_depth_level")

    initial_depth_level = nodes_depth_keys |> Enum.min()
    |> debug("initial_depth_level")

    initial_list = Map.get(nodes_depth_map, initial_depth_level)
    initial_nodes_depth_map = Map.delete(nodes_depth_map, initial_depth_level)

    { tree, tree_nodes_count } = Enum.reduce(initial_list, { [], length(initial_list) }, &extract_to_resulting_structure(&1, &2, initial_nodes_depth_map, initial_depth_level, column_name))

    debug(tree_nodes_count, "tree_nodes_count")

    %{
      max_depth: max_depth_level,
      node_count: tree_nodes_count,
      tree: check_nodes_arrangement_correctness(tree, tree_nodes_count, nodes_list)
    }
  end

  defp nodes_by_depth_map([], processed_map, _), do: processed_map
  defp nodes_by_depth_map([node | tail], before_node_processed_map, column_name) do
    path = Map.get(node, column_name, [])
    # |> debug()
    node_depth = depth(node, path)

    node_at_depth = Map.get(before_node_processed_map, node_depth, []) ++ [node]
    after_node_processed_map = Map.put(before_node_processed_map, node_depth, node_at_depth)

    nodes_by_depth_map(tail, after_node_processed_map, column_name)
  end

  defp extract_to_resulting_structure(%{id: id} = node, { list, total_count }, nodes_depth_map, depth_level, column_name) do
    next_depth_level = depth_level + 1

    { node_children, node_children_count } = nodes_depth_map
      |> Map.get(next_depth_level, [])
      |> Enum.filter(fn(possible_children) -> Map.get(possible_children, column_name, []) |> List.last() == id end)
      |> Enum.reduce({ [], total_count }, &extract_to_resulting_structure(&1, &2, nodes_depth_map, next_depth_level, column_name))

    { list ++ [{ node, node_children }], length(node_children) + node_children_count }
  end
  defp extract_to_resulting_structure(node, { list, _total_count }, _nodes_depth_map, _depth_level, _column_name) do
    warn(node, "invalid path node")
    { list, 0 }
  end

  defp check_nodes_arrangement_correctness(tree, tree_nodes_count, nodes_list) do
    nodes_count = length(nodes_list)

    if tree_nodes_count != nodes_count do

      all_nodes_list_ids = nodes_list 
      |> Enum.map(&Map.get(&1, :id))
      # |> dump("all_nodes_list_ids")

      arranged_tree_node_ids = tree 
      |> Enum.map(&get_node_ids_from_tree/1) 
      |> List.flatten()
      # |> dump("arranged_tree_node_ids")

      missing_node_ids = all_nodes_list_ids -- arranged_tree_node_ids

      warn("there seems to be a missing or invalid materialized `path`, resulting in a different count of items (received #{inspect nodes_count} nodes but the arranged tree contains #{inspect tree_nodes_count}) meaning those with ids [#{Enum.join(missing_node_ids, ", ")}] were just appended to the end of the tree")

      tree ++ (
        nodes_list 
        |> Enum.filter(& Map.get(&1, :id) in missing_node_ids)
        |> Enum.map(& {&1, []})
      ) 
      # |> dump("merged tree")
    else
      tree
    end
  end

  defp get_node_ids_from_tree({ node, [] }), do: [node.id]
  defp get_node_ids_from_tree({ node, list }) do
    [node.id, Enum.map(list, &get_node_ids_from_tree(&1))]
  end
end
