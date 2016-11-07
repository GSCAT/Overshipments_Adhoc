library(RODBC)

my_connect <- odbcConnect(dsn= "IP EDWP", uid= my_uid, pwd= my_pwd)
Overages_Git_wcl <- sqlQuery(my_connect, 
                     query = "select
                     a15.BRD_NM,
                     a12.OPR_BRD_STY_ID,
                     a24.BRD_DIV_DESC, 
                     a22.OPR_BRD_DEPT_ID,
                     a22.BRD_DEPT_DESC,
                     a11.mkt_po_id, 
                     max(a110.CD_DTL_ABBR_NM)  Program_Type,
                     CASE
                     WHEN a11.PAR_VENDOR_ID = (-1) THEN a11.VENDOR_ID
                     ELSE a11.PAR_VENDOR_ID END AS MasterVendorID,
                     a21.VENDOR_LEGAL_DESC as PAR_VENDOR_LEGAL_DESC,
                     b21.VENDOR_LEGAL_DESC as VENDOR_LEGAL_DESC,	
                     min(a111.ISO2_CNTRY_CD) ORIGIN_COUNTRY_CODE,
                     Max(a113.YR_NBR *100 + a113.MO_NBR) AS InDC_YR_MO,
                     sum(XPLD_LN_ORD_QTY) as XPLD_LN_ORD_QTY,
                     MAX(x.INV_RCPT_QTY) AS INV_RCPT_QTY,
                     avg(a11.ELC_AMT_USD) ELC_AMT_USD,
                     sum(ELC_AMT_USD * x.INV_RCPT_QTY) as Total_FCST_ELC
                     --Max(x.MAX_IN_DC_DT) as MAX_IN_DC_DT,
                     -- MAX(PLANNED_IN_DC_DATE) as MAX_PL_IN_DC_DT
                     
                     
                     from VIEWORDER.VIUFF_INBND_UNT_FCST_FCT a11
                     
                     /*	 left outer join (Select mkt_po_id as mkt_po_id, max(PO_RCV_QTY) AS PO_RCV_QTY from VIEWORDER.VIUFF_INBND_UNT_FCST_FCT group by mkt_po_id) z
                     on (z.mkt_po_id = a11.MKT_PO_ID)*/
                     /*	 left outer join (select MKT_PO_ID, sum(INV_RCPT_QTY) as INV_RCPT_QTY , Max(IN_DC_DT) as MAX_IN_DC_DT from VIEWORDER.VRRTW_RMS_RCPT_TXN_FCT where PLN_STK_DT between DATE '2015-08-02' and Date '2016-07-30'  group by MKT_PO_ID) x
                     on (x.MKT_PO_ID=a11.MKT_PO_ID)*/
                     RIGHT JOIN (select MKT_PO_ID from VIEWORDER.VRRTW_RMS_RCPT_TXN_FCT where PLN_STK_DT between DATE '2015-08-02' and Date '2016-07-30'  group by MKT_PO_ID) z
                     on (z.mkt_po_id = a11.MKT_PO_ID)
                     
                     left outer join (SELECT SUBSTR(MKT_PO_ID, 1, 5) AS MKT_PO_ID, SUM(INV_RCPT_QTY)AS INV_RCPT_QTY FROM  SRAA_SAND.TEMP_RMS_MKT_PO
                     GROUP BY SUBSTR(MKT_PO_ID, 1, 5)) x
                     on (x.MKT_PO_ID = a11.MKT_PO_ID)
                     
                     left outer join	 VIEWFNDT.TBRSD_BRD_STY_DIM	 a12
                     on 	(a11.BRD_STY_KEY = a12.BRD_STY_KEY)
                     left outer join	 VIEWFNDT.TBSCD_BRD_SCLS_DIM a13
                     on 	(a12.BRD_SCLS_KEY = a13.BRD_SCLS_KEY)
                     left outer join	 VIEWFNDT.TBCSD_BRD_CLS_DIM	a14
                     on 	(a13.BRD_CLS_KEY = a14.BRD_CLS_KEY)
                     
                     left outer join	 VIEWFNDT.TBRDL_BRD_LOOKUP	a15
                     on 	(a11.BRD_KEY = a15.BRD_KEY)
                     left outer join	 VIEWFNDT.TCHNL_CHNL_LOOKUP	a16
                     on 	(a11.CHNL_KEY = a16.CHNL_KEY)
                     left outer join 	VIEWFNDT.TMKTL_MKT_LOOKUP	a19
                     on 	(a11.MKT_KEY = a19.MKT_KEY)
                     left outer join  VIEWFNDT.VRDCL_RLN_DAY_CAL_LKUP   a113
                     on (a113.FIS_CAL_DT = a11.PLANNED_IN_DC_DATE)
                     
                     left outer join	 VIEWORDER.VVLNL_VND_LEGAL_NM_LOOKUP a21
                     on (MasterVendorID = a21.VENDOR_ID)
                     left outer join	 VIEWORDER.VVLNL_VND_LEGAL_NM_LOOKUP b21
                     on (a11.VENDOR_ID = b21.VENDOR_ID)
                     
                     left outer join	 VIEWFNDT.TBDPD_BRD_DEPT_DIM	a22
                     on 	(a14.BRD_DEPT_KEY = a22.BRD_DEPT_KEY)	
                     left outer join	 VIEWFNDT.TBDHL_BRD_DEPT_ALT_HIER_ASSN	a23
                     on 	(a14.BRD_DEPT_KEY = a23.BRD_DEPT_KEY)
                     left outer join	 VIEWFNDT.TBDVD_BRD_DIV_DIM	a24
                     on 	(a23.PAR_HIER_KEY = a24.BRD_DIV_KEY)
                     left outer join	(SELECT OPR_CD_ID, CD_DTL_ABBR_NM
                     FROM VIEWFNDT.TCDTL_CD_DTL_LOOKUP WHERE CD_TYP_ID = 1158 GROUP BY 1,2)	a110
                     on 	(a11.PO_BLD_TYP_CD = a110.OPR_CD_ID)
                     
                     left outer join	VIEWFNDT.TUNLL_UNED_NATION_LOC_LOOKUP	a111
                     on 	(a11.XFR_PT_COUNTRY_CODE = a111.ISO2_CNTRY_CD and 
                     a11.XFR_PT_PLACE_CODE = a111.UNAT_PLC_CD)
                     
                     where PLANNED_STOCKED_DATE between DATE '2015-06-02' and Date '2016-10-30'
                     --AND FCST_QTY IS NOT NULL
                     and XPLD_LN_ORD_QTY is not null
                     --and ACTUAL_X_FACTORY_DATE <> DATE '9999-12-31'
                     and  PLANNED_STOCKED_DATE <> DATE '9999-12-31'
                     and CURRENT_EVENT not like 'CL%'
                     and a11.mkt_po_id <> 'VJ8MV'
                     and a11.LOC_KEY <> 2820
                     --and a110.CD_DTL_ABBR_NM <> 'VMI'
                     and (INV_RCPT_QTY is not null or  XPLD_LN_ORD_QTY <> 0)
                     --AND A11.MKT_PO_ID = 'WJ5SV'
                     GROUP BY  
                     a15.BRD_NM,
                     a12.OPR_BRD_STY_ID,
                     a24.BRD_DIV_DESC,
                     a22.OPR_BRD_DEPT_ID,
                     a22.BRD_DEPT_DESC,
                     MasterVendorID,
                     a21.VENDOR_LEGAL_DESC,
                     b21.VENDOR_LEGAL_DESC, 
                     --a111.ISO2_CNTRY_CD,
                     a11.mkt_po_id
                     
                     order by a15.BRD_NM;")