CREATE DATABASE IF NOT EXISTS `mybooks` CHARACTER SET UTF8;

USE `mybooks`;

CREATE TABLE `authors` (
    `id`	    INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `name`		VARCHAR(100),
    `surname`	VARCHAR(100),
    `origin_id` INT
);

CREATE TABLE `editors` (
    `id`		INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `name`		VARCHAR(100),
    `surname`	VARCHAR(100)
);

CREATE TABLE `translators`
(
    `id`		INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `name`		VARCHAR(100),
    `surname`	VARCHAR(100)
);

CREATE TABLE `genres` (
    `id`			INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `name`	 		VARCHAR(100),
    `description`	TEXT
); 

CREATE TABLE `series` (
    `id`			INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `name`	 		VARCHAR(100),
    `description`	TEXT
); 

CREATE TABLE `publishers` (
    `id`		INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `name`	 	VARCHAR(100),
    `site`		VARCHAR(100),
    `city`		VARCHAR(100)
);

CREATE TABLE `books` (
    `id`			INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `orig_name`	    VARCHAR(100),
    `name`			VARCHAR(100),
    `ISBN`			VARCHAR(100),
    `year`			DATE,
    `note`			TEXT
);

CREATE TABLE `compositions` (
    `id`			INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `name`			VARCHAR(100),
    `annotation`	TEXT
);

CREATE TABLE `rel_composition_authors` (
    `id`				INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `composition_id`    INT NOT NULL,
    `author_id`		    INT NOT NULL,
    FOREIGN KEY (`composition_id`) REFERENCES `compositions`(`id`),
    FOREIGN KEY (`author_id`) REFERENCES `authors`(`id`)
);

CREATE TABLE `rel_composition_translators` (
    `id`				INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `composition_id`	INT NOT NULL,
    `translator_id`	    INT NOT NULL,
    FOREIGN KEY (`composition_id`) REFERENCES `compositions`(`id`),
    FOREIGN KEY (`translator_id`) REFERENCES `translators`(`id`)
);

CREATE TABLE `rel_books_editors` (
    `id`				INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `composition_id`	INT NOT NULL,
    `editor_id`		    INT NOT NULL,
    FOREIGN KEY (`books_id`) REFERENCES `books`(`id`),
    FOREIGN KEY (`editor_id`) REFERENCES `editors`(`id`)
);

CREATE TABLE `rel_composition_genres` (
    `id`					INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `composition_id`	    INT NOT NULL,
    `genre_id`			    INT NOT NULL,
    FOREIGN KEY (`composition_id`) REFERENCES `compositions`(`id`),
    FOREIGN KEY (`genre_id`) REFERENCES `genres`(`id`)
);

CREATE TABLE `rel_book_compositions` (
    `id`				INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `composition_id`	INT NOT NULL,
    `book_id`			INT NOT NULL,
    FOREIGN KEY (`composition_id`) REFERENCES `compositions`(`id`),
    FOREIGN KEY (`book_id`) REFERENCES `books`(`id`)
);

CREATE TABLE `rel_book_series` (
	`id`				INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
   `book_id`			INT NOT NULL,
   `series_id`			INT NOT NULL,
   FOREIGN KEY (`book_id`) REFERENCES `books`(`id`),
   FOREIGN KEY (`series_id`) REFERENCES `series`(`id`)
);

CREATE TABLE `rel_book_publishers` (
    `id`				INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
    `book_id`			INT NOT NULL,
    `publisher_id`		INT NOT NULL,
    FOREIGN KEY (`book_id`) REFERENCES `books`(`id`),
    FOREIGN KEY (`publisher_id`) REFERENCES `publishers`(`id`)
);


INSERT INTO `series` (`name`, `description`) VALUES ('Абсолютное Оружие', 'Научная фантастика');
INSERT INTO `series` (`name`, `description`) VALUES ('Ведьмак', 'Пирключения ведьмака Геральта');
INSERT INTO `series` (`name`, `description`) VALUES ('Дюна', 'Романы Фрэнка Герберта о пустынной планете Аракис');

INSERT INTO `genres` (`name`, `description`) VALUES ('Научная фантастика','');
INSERT INTO `genres` (`name`, `description`) VALUES ('Фэнтази','');

INSERT INTO `publishers` (`name`, `site`, `city`) VALUES ('Издательство АСТ', 'Москва', 'www.ast.ru');
INSERT INTO `publishers` (`name`, `site`, `city`) VALUES ('Издательство Эксмо', 'Москва', 'www.eksmo.ru');

INSERT INTO `authors` (`name`, `surname`, `origin_id`) VALUES ('Андрей', 'Ливадный', 0);
INSERT INTO `authors` (`name`, `surname`, `origin_id`) VALUES ('Анджей', 'Сапковский', 0);
INSERT INTO `authors` (`name`, `surname`, `origin_id`) VALUES ('Фрэнк', 'Герберт', 0);

INSERT INTO `editors` (`name`, `surname`) VALUES ('Т.В.', 'Полонская');
INSERT INTO `editors` (`name`, `surname`) VALUES ('О.В.', 'Панкрашина'); 
INSERT INTO `editors` (`name`, `surname`) VALUES ('Е.', 'Березина'); 

INSERT INTO `translators` (`name`, `surname`) VALUES ('П','Вязникова');
INSERT INTO `translators` (`name`, `surname`) VALUES ('Ю','Соколова');
INSERT INTO `translators` (`name`, `surname`) VALUES ('А','Анваера');
INSERT INTO `translators` (`name`, `surname`) VALUES ('Е.П.','Вайсброт');

INSERT INTO `compositions` (`name`, `annotation`) VALUES ('Последние желание', 'Серия Ведьмак');
INSERT INTO `compositions` (`name`, `annotation`) VALUES ('Владычица озера', 'Серия Ведьмак');
INSERT INTO `compositions` (`name`, `annotation`) VALUES ('Резервный космодром', 'Киберпанк');
INSERT INTO `compositions` (`name`, `annotation`) VALUES ('Дюна', 'Первая трилогия Дюны');
INSERT INTO `compositions` (`name`, `annotation`) VALUES ('Мессия Дюны', 'Первая трилогия Дюны');
INSERT INTO `compositions` (`name`, `annotation`) VALUES ('Дети Дюны', 'Первая трилогия Дюны');
 

