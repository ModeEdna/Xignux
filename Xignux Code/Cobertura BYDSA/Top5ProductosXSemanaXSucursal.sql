/****** Conseguir los 5 productos mas vendidos por semana, por sucursal  ******/
SELECT *FROM
      (SELECT [Sucursal],
		 [NombreSemana],
		 [NombreProducto],
		 CAST(SUM([VentaBruta]) AS int) AS sumaVentaBruta,
		 DENSE_RANK() OVER(PARTITION BY Sucursal, NombreSemana ORDER BY SUM([VentaBruta]) DESC) AS ranking
	 FROM [PRDinterMbk].[dbo].[TT12_VENTA_SEMANAL_DETALLE_PRODUCTO]
	 WHERE Semana >= 22 -- La semana mas reciente es la 26 en Julio 07, 2020.
	 GROUP BY Sucursal, NombreSemana, NombreProducto
	 -- ORDER BY Sucursal, NombreSemana, SUM([VentaBruta]) DESC
	 ) AS x
WHERE x.ranking <= 5
AND Sucursal IS NOT NULL
ORDER BY Sucursal, NombreSemana, sumaVentaBruta DESC