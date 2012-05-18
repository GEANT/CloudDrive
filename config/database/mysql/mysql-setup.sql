delimiter $$

CREATE DATABASE `rightfabric` /*!40100 DEFAULT CHARACTER SET latin1 */$$

CREATE TABLE `invitees` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(150) DEFAULT NULL,
  `by_whom` varchar(150) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1$$

CREATE TABLE `resettokens` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `token` varchar(64) DEFAULT NULL,
  `username` varchar(150) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1$$

CREATE TABLE `users` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(150) DEFAULT NULL,
  `balance` double DEFAULT NULL,
  `role_c` varchar(64) DEFAULT NULL,
  `email` varchar(512) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  `drive_enabled` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1$$



