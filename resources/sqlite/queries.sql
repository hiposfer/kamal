
-- used for a graph representation of the road network
select * from way_node join node on way_node.node = node.id;
