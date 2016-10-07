CREATE  TABLE SRAA_SAND.TEMP_RMS 
AS
 (select DEST_PO_ID, MAX(MKT_PO_ID) AS MKT_PO_ID, SUM(INV_RCPT_QTY) as INV_RCPT_QTY , Max(IN_DC_DT) as MAX_IN_DC_DT from VIEWORDER.VRRTW_RMS_RCPT_TXN_FCT 
where PLN_STK_DT between DATE '2015-08-02' and Date '2016-07-30'  
group by DEST_PO_ID) WITH DATA;