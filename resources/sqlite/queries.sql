
-- used for a graph representation of the road network
select * from way_node join node on way_node.node = node.id;

select * from link join node as source on link.src = source.id
                   join node as destination on link.dst = destination.id

-- src 2708331052, dst 561065

with recursive
 path(src, dst, cost) as (
    values (null, 2708331052, 0)
    union all
    select link.src, link.dst, link.distance + path.cost as cost
     from path join link on link.src = path.dst
     order by cost
 )
select * from path
 -- group by path.src;
