package com.sap.ic.cmh.masterdata.salesorganization.persistency;

import static cds.gen.masterdataservice.MasterDataService_.SALES_ORGANIZATIONS;

import java.util.List;

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

import cds.gen.complaintservice.Complaints_;
import cds.gen.masterdataservice.SalesOrganizations_;

@Component
public class SalesOrganizationRespositoryImpl implements SalesOrganizationRepository {

    public static final Logger logger = LoggerHelper.getLogger(SalesOrganizationRespositoryImpl.class);

    @Autowired
    private PersistenceService db;

    @Override
    public void deleteInactiveSalesOrganization(List<String> recordsToBeDeleted) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteInactiveSalesOrganization");
        CqnDelete delete = Delete.from(SALES_ORGANIZATIONS).where(cc -> cc.ID().in(recordsToBeDeleted));
        long deleteCount = db.run(delete).rowCount();
        logger.info("Records Deleted count: {} " , deleteCount);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "deleteInactiveSalesOrganization");
    }

    @Override
    public Result getSalesOrganizationMap(List<String> salesOrganizations) {
    	LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getSalesOrganizationMap");
        CqnSelect salesOrganizationSelect = Select.from(SalesOrganizations_.class).columns("ID", "salesOrganization")
                .where(b -> b.salesOrganization().in(salesOrganizations));
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getSalesOrganizationMap");
       return db.run(salesOrganizationSelect);
       
    }
    
    /**
     * Fetch Sales Org details based on Sales Org code
     */
	@Override
	public Result getSalesOrganizationDetailsBasedOnSalesOrgCode(String salesOrganization) {
		CqnSelect select = Select.from(SALES_ORGANIZATIONS).where(b -> b.salesOrganization().eq(salesOrganization));
		return db.run(select);
	}

	/**
	 * Get active complaints with a distribution channel
	 */
	@Override
	public Result getActiveComplaintsInSalesOrganizations(List<String> salesOrganizationsId) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getActiveComplaintsInSalesOrganizations");
        CqnSelect complaintsSelect = Select.from(Complaints_.class).where(b -> b.companyCode_ID()
        		.in(salesOrganizationsId).and(b.complaintStatus_code()
                .ne(Constants.COMPLAINT_CLOSED)));
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getActiveComplaintsInSalesOrganizations");
		return db.run(complaintsSelect);
	}
    
	/**
	 * Fetch Sales Org details based on Sales Org ID
	 */
	@Override
	public Result getSalesOrganizationDetailsBasedOnCodeList(List<String> salesOrganization) {
		CqnSelect select = Select.from(SALES_ORGANIZATIONS).where(b -> b.salesOrganization().in(salesOrganization));
		return db.run(select);
	}

    /**
     * Fetch Sales Org details based on sales org ID
	 */
    @Override
    public Result getSalesOrganizationById(String salesOrganizationId){
        return db.run(Select.from(SalesOrganizations_.class).where(b->b.ID().eq(salesOrganizationId)));
    }

}