
-- used for a graph representation of the road network
select * from way_node join node on way_node.node = node.id;

select * from link join node as source on link.src = source.id
                   join node as destination on link.dst = destination.id

-- src 2708331052, dst 561065
-- "sends a radar beacon to know the distance to the destination"
-- This query is only useful to know the cost of the shortest path
-- until the destination. We can use it to display it to the user
-- and as a way to stop the graph_traversal recursive query below
with recursive
 beacon(src, dst, cost) as (
    values (null, 2708331052, 0)
        union all
    select link.src, link.dst, link.distance + beacon.cost as cost
     from beacon
      join link on link.src = beacon.dst
      order by cost
      limit 100000
 )
 select * from beacon where beacon.dst = 1125302338 limit 1;

-- perform a full graph traversal and returns a table with the
-- cost of reaching each node until the destination. This can be
-- used to properly route the user to its destination
with recursive
 graph_traversal(src, dst, cost) as (
    values (null, 2708331052, 0)
        union all
    select link.src, link.dst, round(link.distance + graph_traversal.cost) as cost
     from graph_traversal
      join link on link.src = graph_traversal.dst
      where round(link.distance + graph_traversal.cost) <= 760
      order by cost
 )
 select * from graph_traversal;
