package com.rh.ai.utilities.nicor.controller;

import static org.springframework.http.HttpStatus.OK;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;

import java.io.IOException;

import javax.annotation.PostConstruct;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import com.rh.ai.utilities.nicor.model.AvgTempSettingInputModel;
import com.rh.ai.utilities.nicor.model.AvgTempSettingPrediction;

import hex.genmodel.MojoModel;
import hex.genmodel.easy.EasyPredictModelWrapper;
import hex.genmodel.easy.RowData;
import hex.genmodel.easy.prediction.RegressionModelPrediction;
import io.swagger.annotations.Api;

@RestController
@RequestMapping("/nicorusage")
@Api("Nicor Analysis")
public class NicorAppController {

    private EasyPredictModelWrapper nicorAppModel;

    @GetMapping("/version")
    public String version() {

        return "0.0.1";
    }

    @PostMapping(path = "/predict",consumes = APPLICATION_JSON_VALUE , produces = APPLICATION_JSON_VALUE)
    public ResponseEntity<AvgTempSettingPrediction> predict(@RequestBody AvgTempSettingInputModel dv) throws Exception {

        RowData rowData = new RowData();
        rowData.put("heatingtimes", dv.getHeatingtimes());
        rowData.put("avghumidity",dv.getAvghumidity());
        rowData.put("avgtmax",dv.getAvgtmax());
        rowData.put("avgtmin", dv.getAvgtmin());
        rowData.put("ccfs", dv.getCcfs());
        rowData.put("currentcharges", dv.getCurrentcharges());
        rowData.put("daysused", dv.getDaysused());
        rowData.put("meterreading", dv.getMeterreading());
        rowData.put("naturalgas", dv.getNaturalgascost());
        rowData.put("tempdiff", dv.getTempdiff());
        
        RegressionModelPrediction regressionModelPrediction = predictAvgTemps(rowData);

        return new ResponseEntity<>(getNicorAppPrediction(regressionModelPrediction), OK);

    }

    @PostConstruct
    public void postConstruct() {
    	
    	try {
    		nicorAppModel = new EasyPredictModelWrapper(MojoModel.load(".\\nicoranalysis.zip"));    
    	} catch (IOException ex) {
    		
    	}
    }

    private RegressionModelPrediction predictAvgTemps (RowData row) throws Exception {
        return nicorAppModel.predictRegression(row);
    }

    private AvgTempSettingPrediction getNicorAppPrediction( RegressionModelPrediction p) {

       AvgTempSettingPrediction prediction = new AvgTempSettingPrediction();
        
       prediction.setAvgtemp(p.value);

       return prediction;
    }



}
