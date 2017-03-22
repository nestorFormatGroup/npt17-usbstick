SET ECHO OFF NEWPAGE 0 SPACE 0 PAGESIZE 0 FEED OFF HEAD OFF TRIMSPOOL ON
SPOOL table_cft_afe_crate_v2.dat

SELECT
              id
   || ',	' || ref_id
   || ',	' || crate_serial_no
   || ',	' || epics_name
   || ',	' || rm_epics_name
   FROM cft_afe_crate_v2;
--WHERE  col2 = 'XYZ';

SPOOL OFF
