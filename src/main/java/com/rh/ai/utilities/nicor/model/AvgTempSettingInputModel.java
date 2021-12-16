package com.rh.ai.utilities.nicor.model;

import lombok.*;

import java.io.Serializable;

@NoArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
@ToString
public class AvgTempSettingInputModel implements Serializable {

	private Double heatingtimes = 0.0;
	private Double avgtmax = 0.0;
	private Double avgtmin = 0.0;
	private Double avghumidity = 0.0;
	private Double daysused = 0.0;
	private Double meterreading = 0.0;
	private Double ccfs = 0.0;
	private Double currentcharges = 0.0;
	private Double naturalgascost = 0.0;
	private Double tempdiff = 0.0;

	public Double getHeatingtimes() {
		return heatingtimes;
	}
	public void setHeatingtimes(Double heatingtimes) {
		this.heatingtimes = heatingtimes;
	}
	public Double getAvgtmax() {
		return avgtmax;
	}
	public void setAvgtmax(Double avgtmax) {
		this.avgtmax = avgtmax;
	}
	public Double getAvgtmin() {
		return avgtmin;
	}
	public void setAvgtmin(Double avgtmin) {
		this.avgtmin = avgtmin;
	}
	public Double getAvghumidity() {
		return avghumidity;
	}
	public void setAvghumidity(Double avghumidity) {
		this.avghumidity = avghumidity;
	}
	public Double getDaysused() {
		return daysused;
	}
	public void setDaysused(Double daysused) {
		this.daysused = daysused;
	}
	public Double getMeterreading() {
		return meterreading;
	}
	public void setMeterreading(Double meterreading) {
		this.meterreading = meterreading;
	}
	public Double getCcfs() {
		return ccfs;
	}
	public void setCcfs(Double ccfs) {
		this.ccfs = ccfs;
	}
	public Double getCurrentcharges() {
		return currentcharges;
	}
	public void setCurrentcharges(Double currentcharges) {
		this.currentcharges = currentcharges;
	}
	public Double getNaturalgascost() {
		return naturalgascost;
	}
	public void setNaturalgascost(Double naturalgascost) {
		this.naturalgascost = naturalgascost;
	}
	public Double getTempdiff() {
		return tempdiff;
	}
	public void setTempdiff(Double tempdiff) {
		this.tempdiff = tempdiff;
	}

}