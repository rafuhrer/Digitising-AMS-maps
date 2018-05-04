/**********************************************************************************************
MERGE LINES AT DEGREE-2 NODES

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	27/04/2014

Description:	Merges adjecent lines (but not at intersections)

Notes:		Requires ''inputlines'' table on DB
**********************************************************************************************/

-- Create candidate lines from inputlines
CREATE TABLE SCHEMANAME.candidatelines AS 
SELECT DISTINCT
	il.lid,
	il.the_geom,
	FALSE AS done
FROM	SCHEMANAME.inputlines il;

-- Create function merging lines (but not at intersections)
DROP FUNCTION IF EXISTS SCHEMANAME.my_union();
create function SCHEMANAME.my_union() returns void
as $$
declare
	lid_i text;
	mergeline geometry;
	the_geom_i geometry;
	startpoint_i geometry;
	endpoint_i geometry;
	startcount int;
	endcount int;
	newline geometry;
	startmatch_lid text;
	startmatch_geom geometry;
	endmatch_lid text;
	endmatch_geom geometry;
	startdone boolean;
	enddone boolean;
	nrows int;
	multiline_i boolean;
begin
	loop

	--select count(lid) into nrows from SCHEMANAME.candidatelines where not done;
	--RAISE NOTICE 'rows left %', nrows;
	
	-- Fetch row from the table which is not done
		select lid, the_geom
		into lid_i, the_geom_i
		from SCHEMANAME.candidatelines where not done limit 1;

		-- If no such rows, algorithm is complete
		exit when not found;

		-- process startpoints
		startdone := TRUE;
		startpoint_i := ST_STARTPOINT(the_geom_i);
		SELECT count(distinct lid) FROM SCHEMANAME.candidatelines INTO startcount WHERE lid != lid_i AND ST_INTERSECTS(startpoint_i, the_geom);
		if (startcount = 1) then 
			SELECT lid, the_geom FROM SCHEMANAME.candidatelines INTO startmatch_lid, mergeline WHERE lid != lid_i AND ST_INTERSECTS(startpoint_i, the_geom);
			multiline_i := GEOMETRYTYPE(ST_LINEMERGE(ST_UNION(mergeline, the_geom_i))) = 'MULTILINESTRING';
			if (not multiline_i) then
				newline := ST_LINEMERGE(ST_UNION(mergeline, the_geom_i));
				UPDATE SCHEMANAME.candidatelines SET the_geom = newline WHERE lid = lid_i;
				the_geom_i := newline;
				DELETE FROM SCHEMANAME.candidatelines WHERE lid = startmatch_lid;
				startdone := FALSE;
			end if;
		end if;
		
		-- process endpoints 
		enddone := TRUE;
		endpoint_i := ST_ENDPOINT(the_geom_i);
		SELECT count(distinct lid) FROM SCHEMANAME.candidatelines INTO endcount WHERE lid != lid_i AND ST_INTERSECTS(endpoint_i, the_geom);
		if (endcount = 1) then 
			SELECT lid, the_geom FROM SCHEMANAME.candidatelines INTO endmatch_lid, mergeline WHERE lid != lid_i AND ST_INTERSECTS(endpoint_i, the_geom);
			multiline_i := GEOMETRYTYPE(ST_LINEMERGE(ST_UNION(mergeline, the_geom_i))) = 'MULTILINESTRING';
			if (not multiline_i) then
				newline := ST_LINEMERGE(ST_UNION(mergeline, the_geom_i));
				UPDATE SCHEMANAME.candidatelines SET the_geom = newline WHERE lid = lid_i;
				the_geom_i := newline;
				DELETE FROM SCHEMANAME.candidatelines WHERE lid = endmatch_lid;
				enddone := FALSE;
			end if;
		end if;

		-- set row to done if no more additional contiguous lines
		if (startdone and enddone) then
			update SCHEMANAME.candidatelines set done = True where lid = lid_i;
		end if;
	end loop;
end;
$$ language plpgsql; 

select SCHEMANAME.my_union();
CREATE TABLE SCHEMANAME.outputlines AS SELECT lid, the_geom FROM SCHEMANAME.candidatelines;

-- Clean up
DROP TABLE SCHEMANAME.inputlines;
DROP TABLE SCHEMANAME.candidatelines;