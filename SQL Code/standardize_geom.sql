/**********************************************************************************************
SNAP INPUT TABLE GEOMETRIES TO GRID

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	25/04/2014

Description:	Reduces precision in the input table geometries to some standardized value.
**********************************************************************************************/

DROP TABLE IF EXISTS SCHEMANAME.newgeoms;
CREATE TABLE SCHEMANAME.newgeoms AS
SELECT
	lid,
	ST_SnapToGrid(the_geom, 0.001) AS the_geom
FROM	SCHEMANAME.inputgeoms
WHERE ST_SnapToGrid(the_geom, 0.001) IS NOT NULL;