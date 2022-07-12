package com.sap.ic.cmh.masterdata.companycode.repository;

import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Complaints_;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.CompanyCodes_;
import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.StructuredType;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
public class CompanyCodeRepositoryImpl implements CompanyCodeRepository {
    public static final Logger logger = LoggerHelper.getLogger(CompanyCodeRepositoryImpl.class);

    @Autowired
    private PersistenceService db;
    @Autowired
    Messages messages;

    @Override
    public Map<String, String> getCompanyCodeMap(List<String> companyCodes) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getCompanyCodeMap");
        CqnSelect companyCodeSelect = Select.from(CompanyCodes_.class).columns("ID", "companyCode")
                .where(b -> b.companyCode().in(companyCodes));
        List<CompanyCodes> companyCodeList = db.run(companyCodeSelect).listOf(CompanyCodes.class);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getCompanyCodeMap");
        return companyCodeList.stream().collect(Collectors.toMap(CompanyCodes::getId, CompanyCodes::getCompanyCode));
    }

    @Override
    public List<String> getActiveComplaintsInCompanyCode(List<String> companyCodesId) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getActiveComplaintsInCompanyCode");
        List<String> complaintsList = new ArrayList<>();
        CqnSelect complaintsSelect = Select.from(Complaints_.class).columns("companyCode_ID").where(b -> b.companyCode_ID().in(companyCodesId)
                        .and(b.complaintStatus_code().ne(Constants.COMPLAINT_CLOSED)));
        final List<Complaints> complaints = db.run(complaintsSelect).listOf(Complaints.class);
        complaintsList.addAll(complaints.stream().map(Complaints::getCompanyCodeId).collect(Collectors.toList()));
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getActiveComplaintsInCompanyCode");
        return complaintsList;
    }

    @Override
    public void deleteCompanyCodeList(List<String> recordsToBeDeleted) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteInactiveCompanyCode");
        CqnDelete delete = Delete.from(CompanyCodes_.class).where(cc -> cc.ID().in(recordsToBeDeleted));
        long deleteCount = db.run(delete).rowCount();
        logger.info("CompanyCodes deleted count : ", deleteCount);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "deleteInactiveCompanyCode");
    }
    @Override
    public String getCompanyCodeIdBasedOnCode(String companyCode){
         LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getCompanyCodeIdBasedOnCode");
         Result companyCodeResult =db.run(Select.from(CompanyCodes_.class).columns(CompanyCodes.ID)
                .where(b -> b.companyCode().eq(companyCode)));
        return companyCodeResult.first().isPresent() ? companyCodeResult.list().get(0).get(CompanyCodes.ID).toString() : "";
    }
    
    /**
     * This method is used to get the CompanyCode details
     *
     * @param companyCode
     * @return CompanyCodes
     */
	@Override
	public <E extends StructuredType<E>> Result fetchCompanyCode(String companyCode, String message,
			Class<E> targetClass, Function<E, Object> targetClassAttribute) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "fetchCompanyCode");
		Result aCompanyCode = null;
		if (!ObjectUtils.isEmpty(companyCode)) {
			aCompanyCode = (db.run(Select.from(CompanyCodes_.class).where(b -> b.companyCode().eq(companyCode))));
		} else {
			messages.error(message).target("in", targetClass, targetClassAttribute);
		}
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "fetchCompanyCode");
		return aCompanyCode;
	}
    /**
     * This method is used to get the CompanyCode details based on Company Code
     * @param companyCode
     * @return
     */
	@Override
	public Result fetchCompanyCodesBasedOnCode(String companyCode) {
		return db.run(Select.from(CompanyCodes_.class).where(b -> b.companyCode().eq(companyCode)));
	}
}
