CREATE  TABLE SRAA_SAND.TEMP_RMS_MKT_PO 
AS
 (select 
 CASE
 WHEN a.DEST_PO_ID BETWEEN '0' AND '9999999'  THEN a.DEST_PO_ID 
 WHEN SUBSTR(a.DEST_PO_ID,1,6) NOT BETWEEN '0' AND '9999999'  THEN SUBSTR(a.DEST_PO_ID,1,6) 
 end  AS MKT_PO_ID, 
 
 MAX(INV_RCPT_QTY) as INV_RCPT_QTY  
 
from SRAA_SAND.TEMP_RMS_TDITW a
group by 
a.DEST_PO_ID ,
SUBSTR(a.DEST_PO_ID,1,6) ) WITH DATA
PRIMARY INDEX(MKT_PO_ID);