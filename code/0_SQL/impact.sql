
drop table #articles

SELECT  DISTINCT cw.carnegie_id, ci.OST_BK, ci.Cit_Rel_ALL_iac
	INTO #articles
  FROM BDKozlowski.dbo.institutions_crosswalk_name_id as cw
  LEFT JOIN [WoS].[pex].[Adresse] as addr on addr.[Institution] = cw.name
  LEFT JOIN [WoS].[pex].[Citations_Relatives] as ci on ci.OST_BK = addr.OST_BK
  LEFT JOIN [WoS].[pex].Article as a on a.OST_BK = addr.OST_BK	
  WHERE a.Annee_Bibliographique  between 1980 and 2019 and a.Code_Document  in (1,2,3)

drop table #impact
SELECT  carnegie_id, COUNT(DISTINCT OST_BK) as N, SUM(Cit_Rel_ALL_iac) as sum_cit
	INTO #impact
  FROM #articles
  GROUP BY carnegie_id

  drop table BDKozlowski.dbo.impact

  SELECT carnegie_id, N as npapers, sum_cit as impact, CAST(sum_cit AS DECIMAL)/N as avg_citations
  into BDKozlowski.dbo.impact
  from #impact
  ORDER BY avg_citations DESC

  select * from  BDKozlowski.dbo.impact