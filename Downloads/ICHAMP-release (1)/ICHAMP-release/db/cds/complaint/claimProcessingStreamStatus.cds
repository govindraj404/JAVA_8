namespace com.sap.ic.cmh.claimProcessingStreamStatus;

using {com.sap.ic.cmh.claimStatus.ClaimStatus} from '../claim/index';
using {com.sap.ic.cmh.streamStatus.StreamStatus } from './index';

type ClaimProcessingStreamStatus : Association to one ClaimProcessingStreamStatuses;

entity ClaimProcessingStreamStatuses {
    key claimStatus : ClaimStatus;
    key streamStatus : StreamStatus;
}