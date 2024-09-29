CREATE TABLE messy (
	unnamed_col VARCHAR(50),
	status VARCHAR(50),
	Los_Angeles INT,
	Phoenix INT,
	San_Diego INT,
	San_Francisco INT,
	Seattle INT
); 

INSERT INTO messy (unnamed_col, status, Los_Angeles, Phoenix, San_Diego, San_Francisco, Seattle)
VALUES
("ALASKA", "on time", 497, 221, 212, 503, 1841),
(NULL, "delayed", 62, 12, 20, 102, 305),
(NULL, NULL, NULL,NULL,NULL,NULL ,NULL ),
("AM WEST", "on time", 694, 4840, 383, 320, 201),
(NULL, "delayed", 117, 415, 65, 129, 61);

SELECT * FROM messy;
