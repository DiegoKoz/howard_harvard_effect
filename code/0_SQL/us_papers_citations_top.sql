SELECT ids.[ID_Art],cit.Cit_2_iac,ti.Top10CR, ti.Top5CR, ti.Top1CR
  FROM BDKozlowski.dbo.us_papers_id_art as ids
  INNER JOIN [Pub_Expanded].[dbo].[TopImpact] as ti on ids.[ID_Art] = ti.Id_art
  INNER JOIN [Pub_Expanded].[dbo].[Citations_Relatives] as cit on ids.ID_Art = cit.ID_Art
