package com.sap.ic.cmh.pdm.handler;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Struct;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.request.ParameterInfo;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.pdmbtpuserservice.BTPUsers;
import cds.gen.pdmbtpuserservice.BTPUsers_;

@Component
@ServiceName("PdmBtpUserService")
public class PdmBtpUserHandler implements EventHandler {

	@Autowired
	ComplaintService complaintService;

	public static final Logger logger = LoggerHelper.getLogger(PdmBtpUserHandler.class);

	@Before(event = { CdsService.EVENT_READ }, entity = BTPUsers_.CDS_NAME)
	public List<BTPUsers> beforeBTPUsersRead(CdsReadEventContext context) {
		logger.info("*********************Before entered****************");
		ParameterInfo parameterInfo = context.getParameterInfo();
		Map<String, String> map1 = parameterInfo.getQueryParams();
		String filter = map1.get("$filter");
		logger.info("***BTP users filtered*** {}  ", filter);
		List<BTPUsers> btpUsersList = new ArrayList<>();

		List<cds.gen.complaintservice.BTPUsers> allResponsiblePerson = complaintService.getAllResponsiblePerson();
		List<BTPUsers> list = !CollectionUtils.isEmpty(allResponsiblePerson) ?  convertComplaintBTPUsersToBTPUsers(allResponsiblePerson)
				: new ArrayList<>();
		logger.info("****BTP Users list**** {} ", list.size());

		if (null != filter) {
			String filteredParamValue = filter.split("'")[1];
			String replaceAll = filteredParamValue.replace("'", "");
			logger.info("**Filtered value after replace** {} ", replaceAll);
			String attribute = filter.split(" ")[0];
			logger.info("**Filtered attribute name ** {} ", attribute);
			List<BTPUsers> collect = list.stream()
					.filter(btp -> null != btp.getPersonResponsibleNumber()
							&& btp.getPersonResponsibleNumber().equalsIgnoreCase(replaceAll)
									? null != btp.getPersonResponsibleNumber()
											&& btp.getPersonResponsibleNumber().equalsIgnoreCase(replaceAll)
									: btp.getPersonResponsibleId().equalsIgnoreCase(replaceAll))
					.collect(Collectors.toList());
			logger.info("****BTP Users Filtered list in before read **** {} ", collect.size());
			btpUsersList.addAll(collect);
		} else {
			btpUsersList.addAll(list);
		}

		return btpUsersList;
	}
	
	
	   /**
		 * Convert non-draft BusinessObjectStatus Structure to draft BusinessObjectStatus
		 * 
		 * @param internalDefects
		 * @return
		 */
		public List<BTPUsers> convertComplaintBTPUsersToBTPUsers(List<cds.gen.complaintservice.BTPUsers> complaintBTPUsers) {
			List<BTPUsers> btpUsersList=new ArrayList<>();
			complaintBTPUsers.forEach(btp->{
				BTPUsers btpUser = Struct
		                .create(BTPUsers.class);
				Map<String, Object> mConvertedData = btp.entrySet().stream()
						.filter(entry -> Objects.nonNull(entry.getValue()))
						.collect(Collectors.toMap(Entry::getKey, Entry::getValue));
				btpUser.putAll(mConvertedData);
				btpUsersList.add(btpUser);
			});
			
			return btpUsersList;
		}
}
