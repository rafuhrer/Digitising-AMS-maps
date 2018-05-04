/**********************************************************************************************
SELECT MOST EFFECTIVE SNAPLINE PER LINE COMBINATION

Author:		Philipp Hunziker (hunziker@icr.gess.ethz.ch)
Last edited:	25/04/2014

Description:	Selects the snap-point combination with the highest "effectiveness" value for each
		line combination.

Notes:		Requires ''snapcandidates'' table on DB
**********************************************************************************************/

-- Select most effective snapline per line combination
CREATE TEMP TABLE scand1 ON COMMIT DROP AS
SELECT DISTINCT ON (lid, lid2)
	*
FROM 		SCHEMANAME.snapcandidates
ORDER BY	lid,
		lid2,
		effectiveness DESC,
		dist;  -- Ensure that if effectiveness is equal, shortest distance snapline is chosen


-- Remove duplicate point combinations (two points may connect more than two lines at intersections)
CREATE TEMP TABLE scand2 ON COMMIT DROP AS
SELECT DISTINCT ON (pid, pid2)
	*
FROM		scand1
ORDER BY	pid,
		pid2,
		effectiveness DESC;


-- Create a unique id identifying point combinations
CREATE TEMP TABLE scand3 ON COMMIT DROP AS
SELECT
	s.*,
	CASE
		WHEN s.pid < s.pid2 THEN s.pid || '-' || s.pid2
		ELSE s.pid2 || '-' || s.pid
	END AS upid
FROM	scand2 s;


-- Remove reverse point combinations (A-B is the same as B-A)
SELECT DISTINCT ON (upid)
	pid,
	pid2,
	dist,
	effectiveness
FROM	scand3
ORDER BY	upid;