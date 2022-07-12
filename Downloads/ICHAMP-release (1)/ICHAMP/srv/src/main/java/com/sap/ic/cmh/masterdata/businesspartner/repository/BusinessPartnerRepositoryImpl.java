package com.sap.ic.cmh.masterdata.businesspartner.repository;

import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Complaints_;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.BusinessPartners_;
import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


@Component
public class BusinessPartnerRepositoryImpl implements BusinessPartnerRepository {
    public static final Logger logger = LoggerHelper.getLogger(BusinessPartnerRepositoryImpl.class);
    private static final String GET_BUSINESS_PARTNERS="getBusinessPartners";

    @Autowired
    private PersistenceService db;

    @Override
    public Map<String, String> getBusinessPartnersMap(List<String> businessPartners) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), GET_BUSINESS_PARTNERS);
        CqnSelect businessPartnerSelect = Select.from(BusinessPartners_.class).columns("ID", "businessPartnerNumber")
                .where(b -> b.businessPartnerNumber().in(businessPartners));
        List<BusinessPartners> businessPartnerList = db.run(businessPartnerSelect).listOf(BusinessPartners.class);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), GET_BUSINESS_PARTNERS);
        return businessPartnerList.stream().collect(Collectors.toMap(BusinessPartners::getId,
                BusinessPartners::getBusinessPartnerNumber));
    }

    @Override
    public List<String> getActiveComplaintsInBusinessPartner(List<String> businessPartnersId) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getActiveComplaintsInBusinessPartner");
        List<String> complaintsList = new ArrayList<>();
        CqnSelect complaintsSelect = Select.from(Complaints_.class).where(b -> b.supplier_ID().in(businessPartnersId).and(b.complaintStatus_code()
                        .ne(Constants.COMPLAINT_CLOSED)));
        final List<Complaints> complaints = db.run(complaintsSelect).listOf(Complaints.class);
        complaintsList.addAll(complaints.stream().map(Complaints::getSupplierId).collect(Collectors.toList()));
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getActiveComplaintsInBusinessPartner");
        return complaintsList;
    }

    @Override
    public void deleteBusinessPartnerList(List<String> recordsToBeDeleted) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteInactiveBusinessPartnerId");
        CqnDelete delete = Delete.from(BusinessPartners_.class).where(cc -> cc.ID().in(recordsToBeDeleted));
        long deleteCount = db.run(delete).rowCount();
        logger.info("BusinessPartners deleted count: ", deleteCount);
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteInactiveBusinessPartnerId");
    }
    /** Updating the Business Partner 
     * @param updateIsMarkedForDeletion
     */
    @Override
    public void updateBusinessPartner(BusinessPartners updateIsMarkedForDeletion) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(),
                "updateBusinessPartnerMarkedForDeletion");
        CqnUpdate update = Update.entity(BusinessPartners_.class).data(updateIsMarkedForDeletion);
        long updateCount = db.run(update).rowCount();
        logger.info("BusinessPartners deleted count: ", updateCount);
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(),
                "updateBusinessPartnerMarkedForDeletion");

    }

    @Override
    public Result getBusinessPartners(String businessPartnerId) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), GET_BUSINESS_PARTNERS);
        return db.run(Select.from(BusinessPartners_.class).where(b -> b.ID().eq(businessPartnerId)));
    }

	@Override
	public Result getBusinessPartnersBasedOnNumber(String businessPartnerNumber) {
		 LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getBusinessPartnersBasedOnNumber");
		return db.run(Select.from(BusinessPartners_.class).where(b -> b.businessPartnerNumber().eq(businessPartnerNumber)));
	}

	@Override
	public Result checkIfCustomerCodeExists(String customerCode) {
		return db.run(Select.from(BusinessPartners_.class).columns(BusinessPartners.CUSTOMER_CODE)
				.where(b -> b.customerCode().eq(customerCode)));
	}

	@Override
	public Result checkIfVendorCodeExists(String vendorCode) {
		return db.run(Select.from(BusinessPartners_.class).columns(BusinessPartners.VENDOR_CODE)
				.where(b -> b.vendorCode().eq(vendorCode)));
	}
}
