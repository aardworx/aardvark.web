namespace Aardvark.Data

open System
open System.Collections.Generic

module Durable =

    type Def = {
        Id : Guid
        Name : string
        Description : string
        Type : Guid
        }

    let private defs = Dictionary<Guid, Def>()
    let private addDef x = defs.Add(x.Id, x)

    let get x = defs.[x]

    module Primitives =

        /// Unit.
        let Unit = {
            Id = Guid.Empty
            Name = "Unit"
            Description = "Unit (nothing, none, null, ...)."
            Type = Guid.Empty
            }
        addDef Unit

        /// Array of elements. Number of elements is known.
        let Array = {
            Id = Guid("5188aaab-7401-4b0e-aca8-fc6b8c5dcfb4")
            Name = "Array"
            Description = "Array of elements. Number of elements is known."
            Type = Guid.Empty
            }
        addDef Array

        /// A map of key/value pairs, where keys are durable IDs with values of corresponding types.
        let DurableMap = {
            Id = Guid("f03716ef-6c9e-4201-bf19-e0cabc6c6a9a")
            Name = "DurableMap"
            Description = "A map of key/value pairs, where keys are durable IDs with values of corresponding types."
            Type = Guid.Empty
            }
        addDef DurableMap

        /// Globally unique identifier (GUID, 16 bytes). https://tools.ietf.org/html/rfc4122.
        let guid = {
            Id = Guid("a81a39b0-8f61-4efc-b0ce-27e2c5d3199d")
            Name = "guid"
            Description = "Globally unique identifier (GUID, 16 bytes). https://tools.ietf.org/html/rfc4122."
            Type = Unit.Id
            }
        addDef guid
        /// Array of globally unique identifiers (GUID, 16 bytes). https://tools.ietf.org/html/rfc4122.
        let GuidArray = {
            Id = Guid("8b5659cd-8fea-46fd-a9f2-52c31bdaf6b3")
            Name = "guid[]"
            Description = "Array of globally unique identifiers (GUID, 16 bytes). https://tools.ietf.org/html/rfc4122."
            Type = Array.Id
            }
        addDef GuidArray

        /// Signed 8-bit integer. 2-complement.
        let Int8 = {
            Id = Guid("47a73639-7ff3-423a-9562-2561d0f51949")
            Name = "int8"
            Description = "Signed 8-bit integer. 2-complement."
            Type = Unit.Id
            }
        addDef Int8
        /// Signed 8-bit integer array. 2-complement.
        let Int8Array = {
            Id = Guid("1e36f786-1c8d-4c1a-b5dd-6f83bfd65287")
            Name = "int8[]"
            Description = "Signed 8-bit integer array. 2-complement."
            Type = Array.Id
            }
        addDef Int8Array

        /// Signed 16-bit integer. 2-complement.
        let Int16 = {
            Id = Guid("4c3f7ded-2037-4f3d-baa9-3a76ef3a1fda")
            Name = "int16"
            Description = "Signed 16-bit integer. 2-complement."
            Type = Unit.Id
            }
        addDef Int16
        /// Signed 16-bit integer array. 2-complement.
        let Int16Array = {
            Id = Guid("80b7028e-e7c8-442c-8ae3-517bb2df645f")
            Name = "int16[]"
            Description = "Signed 16-bit integer array. 2-complement."
            Type = Array.Id
            }
        addDef Int16Array

        /// Signed 32-bit integer. 2-complement.
        let Int32 = {
            Id = Guid("5b4cdca5-8132-4feb-a882-46d4a03c8fde")
            Name = "int32"
            Description = "Signed 32-bit integer. 2-complement."
            Type = Unit.Id
            }
        addDef Int32
        /// Signed 32-bit integer array. 2-complement.
        let Int32Array = {
            Id = Guid("ad5edb27-f7d5-4218-afe4-c0abdcd93703")
            Name = "int32[]"
            Description = "Signed 32-bit integer array. 2-complement."
            Type = Array.Id
            }
        addDef Int32Array

        /// Signed 64-bit integer. 2-complement.
        let Int64 = {
            Id = Guid("f0909b36-d3c4-4b86-8320-e0ad418226e5")
            Name = "int64"
            Description = "Signed 64-bit integer. 2-complement."
            Type = Unit.Id
            }
        addDef Int64
        /// Signed 32-bit integer array. 2-complement.
        let Int64Array = {
            Id = Guid("39761157-4817-4dbf-9eda-33fad1c0a852")
            Name = "int64[]"
            Description = "Signed 64-bit integer array. 2-complement."
            Type = Array.Id
            }
        addDef Int64Array

        /// Floating point value (single precision, 32-bit). https://en.wikipedia.org/wiki/IEEE_754.
        let Float32 = {
            Id = Guid("23fb286f-663b-4c71-9923-7e51c500f4ed")
            Name = "float32"
            Description = "Floating point value (single precision, 32-bit). https://en.wikipedia.org/wiki/IEEE_754."
            Type = Unit.Id
            }
        addDef Float32
        /// Array of floating point values (single precision, 32-bit). https://en.wikipedia.org/wiki/IEEE_754.
        let Float32Array = {
            Id = Guid("a687a789-1b63-49e9-a2e4-8099aa7879e9")
            Name = "float32[]"
            Description = "Array of floating point values (single precision, 32-bit). https://en.wikipedia.org/wiki/IEEE_754."
            Type = Array.Id
            }
        addDef Float32Array

        /// Floating point value (single precision, 64-bit). https://en.wikipedia.org/wiki/IEEE_754.
        let Float64 = {
            Id = Guid("c58c9b83-c2de-4153-a588-39c808aed50b")
            Name = "float64"
            Description = "Floating point value (single precision, 64-bit). https://en.wikipedia.org/wiki/IEEE_754."
            Type = Unit.Id
            }
        addDef Float64
        /// Array of floating point values (single precision, 64-bit). https://en.wikipedia.org/wiki/IEEE_754.
        let Float64Array = {
            Id = Guid("ba60cc30-2d56-45d8-a051-6b895b51bb3f")
            Name = "float64[]"
            Description = "Array of floating point values (single precision, 64-bit). https://en.wikipedia.org/wiki/IEEE_754."
            Type = Array.Id
            }
        addDef Float64Array

        /// Aardvark.Base.V3f.
        let V3f = {
            Id = Guid("ad8adcb6-8cf1-474e-99da-851343858935")
            Name = "Aardvark.Base.V3f"
            Description = "Aardvark.Base.V3f."
            Type = Unit.Id
            }
        addDef V3f
        /// Aardvark.Base.V3f[].
        let V3fArray = {
            Id = Guid("f14f7607-3ddd-4e52-9ff3-c877c2242021")
            Name = "Aardvark.Base.V3f[]"
            Description = "Aardvark.Base.V3f[]."
            Type = Array.Id
            }
        addDef V3fArray

        /// Aardvark.Base.V3d.
        let V3d = {
            Id = Guid("7a0be234-ab45-464d-b706-87157aba4361")
            Name = "Aardvark.Base.V3d"
            Description = "Aardvark.Base.V3d."
            Type = Unit.Id
            }
        addDef V3d
        /// Aardvark.Base.V3d[].
        let V3dArray = {
            Id = Guid("2cce99b6-e823-4b34-8615-f7ab88746554")
            Name = "Aardvark.Base.V3d[]"
            Description = "Aardvark.Base.V3d[]."
            Type = Array.Id
            }
        addDef V3dArray

        /// Aardvark.Base.Box3f.
        let Box3f = {
            Id = Guid("416721ca-6df1-4ada-b7ad-1da7256f490d")
            Name = "Aardvark.Base.Box3f"
            Description = "Aardvark.Base.Box3f."
            Type = Unit.Id
            }
        addDef Box3f

        /// Aardvark.Base.Box3d.
        let Box3d = {
            Id = Guid("5926f1ce-37fb-4022-a6e5-536b22ad79ea")
            Name = "Aardvark.Base.Box3d"
            Description = "Aardvark.Base.Box3d."
            Type = Unit.Id
            }
        addDef Box3d

        /// Aardvark.Base.Cell.
        let Cell = {
            Id = Guid("bb9da8cb-c9d6-43dd-95d6-f569c82d9af6")
            Name = "Aardvark.Base.Cell"
            Description = "Aardvark.Base.Cell."
            Type = Unit.Id
            }
        addDef Cell

        /// Aardvark.Base.C3b.
        let C3b = {
            Id = Guid("73656667-ea6a-468f-962c-64cd4e24f409")
            Name = "Aardvark.Base.C3b"
            Description = "Aardvark.Base.C3b."
            Type = Unit.Id
            }
        addDef C3b
        /// Aardvark.Base.C3b[].
        let C3bArray = {
            Id = Guid("41dde1c8-2b63-4a18-90c8-8f0c67c685b7")
            Name = "Aardvark.Base.C3b[]"
            Description = "Aardvark.Base.C3b[]."
            Type = Array.Id
            }
        addDef C3bArray
   
    module Octree =
        /// Octree. An octree node. DurableMap.
        let Buffer = {
            Id = Guid("e72d4eb6-4060-4d87-8877-bfce1e9f6a30")
            Name = "Octree.Buffer"
            Description = "Original data DurableMap."
            Type = Primitives.Unit.Id
            }
        addDef Buffer

        /// Octree. An octree node. DurableMap.
        let Node = {
            Id = Guid("e0883944-1d81-4ff5-845f-0b96075880b7")
            Name = "Octree.Node"
            Description = "Octree. An octree node. DurableMap."
            Type = Primitives.DurableMap.Id
            }
        addDef Node

        /// Octree. An octree node followed by all other nodes in depth first order. DurableMap.
        let Tree = {
            Id = Guid("708b4b8e-286b-4658-82ac-dd8ea98d1b1c")
            Name = "Octree.Tree"
            Description = "Octree. An octree node followed by all other nodes in depth first order. DurableMap."
            Type = Primitives.DurableMap.Id
            }
        addDef Tree

        /// Octree. Node's unique id.
        let NodeId = {
            Id = Guid("1100ffd5-7789-4872-9ef2-67d45be0c489")
            Name = "Octree.NodeId"
            Description = "Octree. Node's unique id."
            Type = Primitives.guid.Id
            }
        addDef NodeId

        /// Octree. Subnodes as array of guids. Array length is 8 for inner nodes (where Guid.Empty means no subnode) and length 0 for leaf nodes.
        let SubnodesGuids = {
            Id = Guid("eb44f9b0-3247-4426-b458-1b6e9880d466")
            Name = "Octree.Subnodes.Guids"
            Description = "Octree. Subnodes as array of guids. Array length is 8 for inner nodes (where Guid.Empty means no subnode) and no array for leaf nodes."
            Type = Primitives.GuidArray.Id
            }
        addDef SubnodesGuids

        /// Octree. Exact bounding box of this node's positions. Global space. Box3d.
        let BoundingBoxExactGlobal = {
            Id = Guid("7912c862-74b4-4f44-a8cd-d11ea1da9304")
            Name = "Octree.BoundingBoxExactGlobal"
            Description = "Octree. Exact bounding box of this node's positions. Global space. Box3d."
            Type = Primitives.Box3d.Id
            }
        addDef BoundingBoxExactGlobal

        /// Octree. Exact bounding box of this node's positions. Local space. Box3f.
        let BoundingBoxExactLocal = {
            Id = Guid("aadbb622-1cf6-42e0-86df-be79d28d6757")
            Name = "Octree.BoundingBoxExactLocal"
            Description = "Octree. Exact bounding box of this node's positions. Local space. Box3f."
            Type = Primitives.Box3f.Id
            }
        addDef BoundingBoxExactLocal

        /// Octree. Cell's index. Global space. Aardvark.Base.Cell.
        let Cell = {
            Id = Guid("9f8121e4-83af-40e3-aed9-5fd908a140ee")
            Name = "Octree.Cell"
            Description = "Octree. Cell's index. Global space. Aardvark.Base.Cell."
            Type = Primitives.Cell.Id
            }
        addDef Cell

        /// Octree. Number of points in this cell. int32.
        let PointCountCell = {
            Id = Guid("172e1f20-0ffc-4d9c-9b3d-903fca41abe3")
            Name = "Octree.PointCountCell"
            Description = "Octree. Number of points in this cell. int32."
            Type = Primitives.Int32.Id
            }
        addDef PointCountCell

        /// Octree. Average distance of each point to its nearest neighbour. float32.
        let AveragePointDistance = {
            Id = Guid("39c21132-4570-4624-afae-6304851567d7")
            Name = "Octree.AveragePointDistance"
            Description = "Octree. Average distance of each point to its nearest neighbour. float32."
            Type = Primitives.Float32.Id
            }
        addDef AveragePointDistance

        /// Octree. Standard deviation of average distances of each point to its nearest neighbour. float32.
        let AveragePointDistanceStdDev = {
            Id = Guid("94cac234-b6ea-443a-b196-c7dd8e5def0d")
            Name = "Octree.AveragePointDistanceStdDev"
            Description = "Octree. Standard deviation of average distances of each point to its nearest neighbour. float32."
            Type = Primitives.Float32.Id
            }
        addDef AveragePointDistanceStdDev

        /// Octree. Min depth of this tree. A leaf node has depth 0. int32.
        let MinTreeDepth = {
            Id = Guid("42edbdd6-a29e-4dfd-9836-050ab7fa4e31")
            Name = "Octree.MinTreeDepth"
            Description = "Octree. Min depth of this tree. A leaf node has depth 0. int32."
            Type = Primitives.Int32.Id
            }
        addDef MinTreeDepth

        /// Octree. Max depth of this tree. A leaf node has depth 0. int32.
        let MaxTreeDepth = {
            Id = Guid("d6f54b9e-e907-46c5-9106-d26cd453dc97")
            Name = "Octree.MaxTreeDepth"
            Description = "Octree. Max depth of this tree. A leaf node has depth 0. int32."
            Type = Primitives.Int32.Id
            }
        addDef MaxTreeDepth

        /// Octree. Per-point positions in global space. V3d[].
        let PositionsGlobal3d = {
            Id = Guid("61ef7c1e-6aeb-45cd-85ed-ad0ed2584553")
            Name = "Octree.PositionsGlobal3d"
            Description = "Octree. Per-point positions in global space. V3d[]."
            Type = Primitives.V3dArray.Id
            }
        addDef PositionsGlobal3d

        /// Octree. Per-point positions in local cell space. V3f[].
        let PositionsLocal3f = {
            Id = Guid("05eb38fa-1b6a-4576-820b-780163199db9")
            Name = "Octree.PositionsLocal3f"
            Description = "Octree. Per-point positions in local cell space. V3f[]."
            Type = Primitives.V3fArray.Id
            }
        addDef PositionsLocal3f

        /// Octree. Per-point normals (V3f[]).
        let Normals3f = {
            Id = Guid("712d0a0c-a8d0-42d1-bfc7-77eac2e4a755")
            Name = "Octree.Normals3f"
            Description = "Octree. Per-point normals (V3f[])."
            Type = Primitives.V3fArray.Id
            }
        addDef Normals3f

        /// Octree. Per-point normals (byte[]).
        let Normals3sb = {
            Id = Guid("aaf4872c-0964-4351-9530-8a3e2be94a6e")
            Name = "Octree.Normals3sb"
            Description = "Octree. Per-point normals (byte[])."
            Type = Primitives.Int8Array.Id
            }
        addDef Normals3sb

        /// Octree. Per-point colors. C3b[].
        let Colors3b = {
            Id = Guid("61cb1fa8-b2e2-41ae-8022-5787b44ee058")
            Name = "Octree.Colors3b"
            Description = "Octree. Per-point colors. C3b[]."
            Type = Primitives.C3bArray.Id
            }
        addDef Colors3b

        /// Octree. Per-point intensities. int32[].
        let Intensities1i = {
            Id = Guid("361027fd-ac58-4de8-89ee-98695f8c5520")
            Name = "Octree.Intensities1i"
            Description = "Octree. Per-point intensities. int32[]."
            Type = Primitives.Int32Array.Id
            }
        addDef Intensities1i

        /// Octree. Kd-tree index array. int32[].
        let KdTreeIndexArray = {
            Id = Guid("c533bd54-9aff-40e1-a2bb-c69c9778fecb")
            Name = "Octree.KdTreeIndexArray"
            Description = "Octree. Kd-tree index array. int32[]."
            Type = Primitives.Int32Array.Id
            }
        addDef KdTreeIndexArray

        
