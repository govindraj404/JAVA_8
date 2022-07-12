package com.sap.ic.cmh.masterdata.division.persistency;

import static cds.gen.masterdataservice.MasterDataService_.DIVISIONS;

import java.util.List;

import cds.gen.masterdataservice.Divisions;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import cds.gen.masterdataservice.Divisions_;
import cds.gen.complaintservice.Complaints_;

@Component
public class DivisionRepositoryImpl implements DivisionRepository {
	public static final Logger logger = LoggerHelper.getLogger(DivisionRepositoryImpl.class);

	@Autowired
	private PersistenceService db;

	@Override
	public void deleteSalesDivision(List<String> recordsToBeDeleted) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "DivisionRepositoryImpl");
		CqnDelete delete = Delete.from(DIVISIONS).where(cc -> cc.ID().in(recordsToBeDeleted));
		long deleteCount = db.run(delete).rowCount();
		logger.info("SalesDivisions deleted count: {}  ", deleteCount);
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "DivisionRepositoryImpl");
	}

	@Override
	public Result getDivisionMap(List<String> divisions, List<String> salesOrgIdList) {
		
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getDivisionMap");
		CqnSelect divisionSelect = Select.from(DIVISIONS)
				.where(b -> b.salesDivision().in(divisions).and(b.salesOrganizationID_ID().in(salesOrgIdList)));
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getDivisionMap");
		return db.run(divisionSelect);
	}

	@Override
	public Result getDivisionDetailsBasedOnDivisionAndSalesOrg(String division, String salesOrganizationId) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(),
				"getDivisionDetailsBasedOnDivisionAndSalesOrg");
		CqnSelect select = Select.from(DIVISIONS)
				.where(b -> b.salesDivision().eq(division).and(b.salesOrganizationID_ID().eq(salesOrganizationId)));
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(),
				"getDivisionDetailsBasedOnDivisionAndSalesOrg");
		return db.run(select);
	}
	
	/**
	 * Get active customer complaints with a Sales division
	 */
	@Override
	public Result getActiveComplaintsInSalesDivisions(List<String> divisionsId) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getActiveComplaintsInDistributionChannels");
        CqnSelect complaintsSelect = Select.from(Complaints_.class).where(b -> b.companyCode_ID()
        		.in(divisionsId).and(b.complaintStatus_code()
                .ne(Constants.COMPLAINT_CLOSED)));
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getActiveComplaintsInDistributionChannels");
		return db.run(complaintsSelect);
	}

    /**
     * Get divisionID and salesOrgID based on ID
	 */
    @Override
    public Result getDivisionIDandSalesIDById(String divisionId){
        return db.run(Select.from(Divisions_.class)
				.columns(Divisions.SALES_ORGANIZATION_ID_ID,Divisions.ID).where(b->b.ID().eq(divisionId)));
    }

}