package com.rh.ai.utilities.nicor.model;

import lombok.*;

import java.io.Serializable;
import java.util.List;

@NoArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
@ToString
public class AvgTempSettingPrediction implements Serializable {

    private Double avgtemp;
    
	public Double getAvgtemp() {
		return avgtemp;
	}
	public void setAvgtemp(Double avgtemp) {
		this.avgtemp = avgtemp;
	}


}