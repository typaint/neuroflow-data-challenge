# 1. How many users completed an exercise in their first month per monthly cohort?
SELECT t1.year, 
		t1.month, 
        CONCAT(ROUND((users_ex / users)*100,2), '%') AS percent
FROM (SELECT MONTH(created_at) AS month,
		YEAR(created_at) AS year,
		COUNT(user_id) AS users
	FROM users 
	GROUP BY YEAR(created_at), MONTH(created_at)) AS t1
JOIN (SELECT MONTH(exercise_completion_date) as month,
		YEAR(exercise_completion_date) AS year,
		COUNT(DISTINCT e.user_id) AS users_ex
		FROM exercises e
		JOIN users USING (user_id)
		WHERE YEAR(exercise_completion_date) = YEAR(created_at) AND
				MONTH(exercise_completion_date) = MONTH(created_at)
		GROUP BY YEAR(exercise_completion_date), MONTH(exercise_completion_date), user_id) AS t2
        ON t1.month = t2.month AND 
        t1.year = t2.year;
        
# 2. How many users completed a given amount of exercises?
SELECT COUNT(user_id) AS users,
		activities
FROM (SELECT user_id, COUNT(exercise_id) AS activities
		FROM exercises e
		GROUP BY user_id) AS t1
GROUP BY activities;

# 3. Which organizations have the most severe patient population?
SELECT organization_name, AVG(score) AS avg_score
FROM providers
JOIN phq9 USING (provider_id)
GROUP BY organization_id, organization_name # assumption that there is no direct functional dependency between organization_id and name
ORDER BY avg_score DESC
LIMIT 5;
