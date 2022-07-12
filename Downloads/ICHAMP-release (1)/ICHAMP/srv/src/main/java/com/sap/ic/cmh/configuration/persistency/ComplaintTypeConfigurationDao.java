package com.sap.ic.cmh.configuration.persistency;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import com.sap.cds.services.persistence.PersistenceService;
import cds.gen.configurationservice.ComplaintTypeConfigurations_;
import java.util.ArrayList;
import java.util.List;
import com.sap.cds.Result;
import com.sap.cds.ql.CQL;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnSelectListItem;

@Repository
public class ComplaintTypeConfigurationDao {

	public static final Logger logger = LoggerHelper.getLogger(ComplaintTypeConfigurationDao.class);
	private static final String COMPLAINT_TYPE_DAO = "ComplaintTypeConfigurationDao";

	@Autowired
	PersistenceService db;

	/**
	 * To perform select query on complaint type configuration
	 */
	public Result getComplaintTypeConfiguration() {
		return db
				.run(Select.from(ComplaintTypeConfigurations_.class).columns(b->b.identifier()).orderBy(c -> c.get("identifier").desc()));
	}

	/**
	 * To perform select query on complaint type configuration based on complaint type code
	 */
	public Result getComplaintTypeConfigurationBasedOnCode(String code) {
		return db
				.run(Select.from(ComplaintTypeConfigurations_.class).columns(b->b.code(),b->b.ID()).where(b->b.code().eq(code)));
	}

	/**
	 * get configurtion types details based on Id
	 * @param {@link String} complaintTypeId
	 * @public
	 */
	public Result getAllComplaintTypesDetails(String complaintTypeId) {
		List<CqnSelectListItem> selectList = new ArrayList<>();
		selectList.add(CQL.to("complaintTypeToSalesAreaMappings").expand());
		selectList.add(CQL.star());
		return db.run(Select.from(ComplaintTypeConfigurations_.class)
				.columns(selectList)
				.where(b->b.ID().eq(complaintTypeId)));
	}

	/**
	 * fetch Complaint Type based on ID.
	 *
	 * @public
	 */
	public Result getComplaintTypeDetailsBasedOnId(String complaintTypeId) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_DAO, "getComplaintTypeDetailsBasedOnId");
		return db.run(Select.from(ComplaintTypeConfigurations_.class).where(b -> b.ID().eq(complaintTypeId)));
	}

}
