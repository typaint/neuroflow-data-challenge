# 1. How many users completed an exercise in their first month per monthly cohort?
SELECT t1.year, 
		t1.month, 
        CONCAT(ROUND((users_ex / users)*100,2), '%') AS percent # divide user exercise cohort by total cohort, convert to %
FROM (SELECT MONTH(created_at) AS month,
		YEAR(created_at) AS year,
		COUNT(user_id) AS users
	FROM users 
	GROUP BY YEAR(created_at), MONTH(created_at)) AS t1 # create cohort of # of users created per year & month
JOIN (SELECT MONTH(exercise_completion_date) as month,
		YEAR(exercise_completion_date) AS year,
		COUNT(DISTINCT e.user_id) AS users_ex # distinct users since users can comlete multiple exercises in cohort month
		FROM exercises e
		JOIN users USING (user_id)
		WHERE YEAR(exercise_completion_date) = YEAR(created_at) AND
				MONTH(exercise_completion_date) = MONTH(created_at)
		GROUP BY YEAR(exercise_completion_date), MONTH(exercise_completion_date), user_id) AS t2 # find users that completed exercises in cohort month
        ON t1.month = t2.month AND 
        t1.year = t2.year;
        
# 2. How many users completed a given amount of exercises?
SELECT COUNT(user_id) AS users,
		activities
FROM (SELECT user_id, COUNT(exercise_id) AS activities
		FROM exercises e
		GROUP BY user_id) AS t1 # calculate # of exercises per user
GROUP BY activities; # find # of users per # of activities completed

# 3. Which organizations have the most severe patient population?
SELECT organization_name, AVG(score) AS avg_score # calculate avg score per organization
FROM providers
JOIN phq9 USING (provider_id)
GROUP BY organization_id, organization_name # assumption that there is no direct functional dependency between organization_id and name
ORDER BY avg_score DESC # biggest to smallest scores
LIMIT 5; # top 5
