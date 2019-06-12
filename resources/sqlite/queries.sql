
-- used for a graph representation of the road network
select * from way_node join node on way_node.node = node.id;

select * from link join node as source on link.src = source.id
                   join node as destination on link.dst = destination.id

-- src 2708331052, dst 561065

with recursive
 path(src, dst, cost, settled) as (
    values (null, 2708331052, 0, 1)
    union all
    select path.dst, link.dst, link.distance, 0
     from path, link
     where link.src = path.dst
 )
select * from path;
