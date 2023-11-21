SELECT ids.[ID_Art], jif.FI_2, jif.FIR_2
	FROM BDKozlowski.dbo.us_papers_id_art as ids
	INNER JOIN [Pub_Expanded].[dbo].[FIRM] as jif on ids.[ID_Art] = jif.ID_Art
