/**********************************************************************************************
ELIMINATE DUPLICATE LINES

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	25/04/2014

Description:	Removes duplicate lines from an inputline table; returns line ids without
		duplicate lines.

Notes:		Requires ''inputlines'' table on DB with id column ''lid''
**********************************************************************************************/

CREATE TEMP TABLE redundantlines ON COMMIT DROP AS
SELECT DISTINCT ON (uniqueid)
	ui.uniqueid,
	ui.lid
FROM
	(SELECT 
		il.lid,
		il2.lid AS lid2,
		CASE 
			WHEN il.lid < il2.lid THEN il.lid || '_' || il2.lid
			ELSE il2.lid || '_' || il.lid
		END AS uniqueid
	 FROM 	SCHEMANAME.inputlines il
		LEFT JOIN SCHEMANAME.inputlines il2
		ON ST_EQUALS(il.the_geom, il2.the_geom)
		AND il.lid <> il2.lid
	WHERE	il2.lid IS NOT NULL) ui;


SELECT
	il.lid
FROM	SCHEMANAME.inputlines il
	LEFT JOIN redundantlines rd
	ON rd.lid = il.lid
WHERE	rd.lid IS NULL;