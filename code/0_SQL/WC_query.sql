/****** HBCU not retrieved from previous search  ******/
SELECT Institution,Province, COUNT(ID_Art)
  FROM [Pub_Expanded].[dbo].[Adresse]
   WHERE Pays = 'USA' AND
   (Institution LIKE '%AGNES-SCOTT%' AND [Province]='GA') OR
(Institution LIKE '%ALVERNO%' AND [Province]='WI') OR
(Institution LIKE '%BAY-PATH%' AND [Province]='MA') OR
(Institution LIKE '%BENNETT%' AND [Province]='NC') OR
(Institution LIKE '%BRENAU%' AND [Province]='GA') OR
(Institution LIKE '%CEDAR-CREST%' AND [Province]='PA') OR
(Institution LIKE '%OF-SAINT-BENEDICT%' AND [Province]='MN') OR
(Institution LIKE '%OF-SAINT-MARY%' AND [Province]='NE') OR
(Institution LIKE '%CONVERSE%' AND [Province]='SC') OR
(Institution LIKE '%COTTEY%' AND [Province]='MO') OR
(Institution LIKE '%HOLLINS%' AND [Province]='VA') OR
(Institution LIKE '%MARY-BALDWIN%' AND [Province]='VA') OR
(Institution LIKE '%MEREDITH%' AND [Province]='NC') OR
(Institution LIKE '%MILLS%' AND [Province]='CA') OR
(Institution LIKE '%MOORE--OF-ART-AND-DESIGN%' AND [Province]='PA') OR
(Institution LIKE '%MOUNT-SAINT-MARYS%' AND [Province]='CA') OR
(Institution LIKE '%NOTRE-DAME-OF-MARYLAND%' AND [Province]='MD') OR
(Institution LIKE '%RUSSELL-SAGE%' AND [Province]='NY') OR
(Institution LIKE '%SAINT-MARYS%' AND [Province]='IN') OR
(Institution LIKE '%SALEM%' AND [Province]='NC') OR
(Institution LIKE '%SCRIPPS%' AND [Province]='CA') OR
(Institution LIKE '%SPELMAN%' AND [Province]='GA') OR
(Institution LIKE '%ST-CATHERINE%' AND [Province]='MN') OR
(Institution LIKE '%STEPHENS%' AND [Province]='MO') OR
(Institution LIKE '%SWEET-BRIAR%' AND [Province]='VA') OR
(Institution LIKE '%TRINITY-WASHINGTON%' AND [Province]='DC') OR
(Institution LIKE '%WESLEYAN%' AND [Province]='GA')
GROUP BY Institution,Province

