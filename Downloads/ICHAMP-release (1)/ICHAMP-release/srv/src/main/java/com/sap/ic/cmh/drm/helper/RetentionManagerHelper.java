package com.sap.ic.cmh.drm.helper;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

public class RetentionManagerHelper {


	private RetentionManagerHelper() {

	}

	private static final String DATE_FORMATER1 = "dd/MM/yyyy";
	private static final String DATE_FORMATER2 = "yyyy-MM-dd";
	private static final String DATE_FORMATER3 = "yyyyMMdd";

	public static Boolean residencyCheck(String residenceDate, String dataSubjectCreatedTime) {

		Date prasedResidenceDate = null;
		Date prasedDateSubjectDate = null;

		try {
			prasedResidenceDate = dateFormater(residenceDate).parse(residenceDate);
			prasedDateSubjectDate = dateFormater(dataSubjectCreatedTime).parse(dataSubjectCreatedTime);
		} catch (ParseException e) {
			return Boolean.FALSE;
		}

		if (prasedResidenceDate.compareTo(prasedDateSubjectDate) > 0
				|| prasedResidenceDate.compareTo(prasedDateSubjectDate) == 0) {
			return Boolean.TRUE;
		} else {
			return Boolean.FALSE;
		}
	}

	public static Boolean isDeleteOrBlock(String retentionDate) throws ParseException {
		if (java.sql.Date.valueOf(LocalDate.now()).compareTo(dateFormater(retentionDate).parse(retentionDate)) > 0) {
			return Boolean.TRUE;
		} else {
			return Boolean.FALSE;
		}
	}

	public static SimpleDateFormat dateFormater(String maxDate) {
		if (maxDate.contains("/")) {
			return new SimpleDateFormat(DATE_FORMATER1);
		} else if (maxDate.contains("-")) {
			return new SimpleDateFormat(DATE_FORMATER2);
		} else {
			return new SimpleDateFormat(DATE_FORMATER3);
		}
	}

	public static String convertLocalDate(String dataSubDate) throws ParseException {
		SimpleDateFormat formater = new SimpleDateFormat(DATE_FORMATER2);
		DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
		LocalDate localDate = new java.sql.Date(formater.parse(dataSubDate).getTime()).toLocalDate();
		LocalDateTime retentionStartDate = localDate.atTime(00, 00, 00);
		return dateTimeFormatter.format(retentionStartDate);
	}
}
