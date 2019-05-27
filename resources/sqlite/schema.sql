PRAGMA foreign_keys=on;

-- <node id="298884269" lat="54.0901746" lon="12.2482632" user="SvenHRO"
--      uid="46882" visible="true" version="1" changeset="676636"
--      timestamp="2008-09-21T21:37:45Z"/>)
create table if not exists node (
    id integer primary key not null,
    lat float not null,
    lon float not null
);

-- <way id="26659127" user="Masch" uid="55988" visible="true" version="5" changeset="4142606"
--    ;timestamp="2010-03-16T11:47:08Z">
--   <nd ref="292403538"/>
--   <nd ref="298884289"/>
--   ...
--   <nd ref="261728686"/>
--   <tag k="highway" v="unclassified"/>
--   <tag k="name" v="Pastower Straße"/>
--  </way>
create table if not exists way (
    id integer primary key not null
);

-- the path that a way follows through the nodes
create table if not exists way_node (
    way integer not null references way,
    node integer not null references node,
    sequence integer not null check (sequence >= 0),

    primary key (way, sequence)
);

create table if not exists way_tag (
    way integer not null references way,
    key text not null,
    value text not null,

    primary key (way, key)
);
