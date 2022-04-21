--Exercise
-- Replace the percentage scaling with min-max scaling.
DROP TABLE IF EXISTS purchases_per_customer; 
CREATE TABLE IF NOT EXISTS purchases_per_customer AS
SELECT cid, SUM(bought) AS total_bought
FROM purchases
GROUP BY cid;

SELECT * FROM purchases_per_customer;
DROP TABLE IF EXISTS combined; 
CREATE TABLE IF NOT EXISTS combined AS
SELECT p.cid, p.itemid, p.bought, ppc.total_bought
FROM purchases p
LEFT JOIN purchases_per_customer ppc
ON p.cid = ppc.cid;

SELECT cid, itemid, bought, total_bought, bought*1.0/total_bought AS scaled_bought
FROM combined;

SELECT * FROM combined;
SELECT cid, itemid, bought, total_bought, bought-MIN(bought)/MAX(bought)-MIN(bought) AS scaled_bought
FROM combined;
