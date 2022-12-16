/****** Conseguir los 5 productos mas vendidos por sucursal  ******/
SELECT *FROM
      (SELECT [Sucursal],
		 [NombreProducto],
		 CAST(SUM([VentaBruta]) AS int) AS sumaVentaBruta,
		 DENSE_RANK() OVER(PARTITION BY Sucursal ORDER BY SUM([VentaBruta]) DESC) AS ranking
	 FROM [PRDinterMbk].[dbo].[TT12_VENTA_SEMANAL_DETALLE_PRODUCTO]
	 WHERE Semana >= 22 -- La semana mas reciente es la 26 en Julio 07, 2020.
	 GROUP BY Sucursal, NombreProducto
	 ) AS x
WHERE x.ranking <= 5
AND Sucursal IS NOT NULL
ORDER BY Sucursal, sumaVentaBruta DESC