package com;

public class TestPojo {
    private String pipelineName;

    private String datasetVersion;

    public TestPojo() {}

    public TestPojo(String pipelineName) {
        this.pipelineName = pipelineName;
    }

    public TestPojo(String pipelineName, String datasetVersion) {
        this.pipelineName = pipelineName;
        this.datasetVersion= datasetVersion;
    }

    public String getPipelineName() {return pipelineName;}

    public String getDatasetVersion() {return datasetVersion;}

    public void setPipelineName(String pipelineName) {
        this.pipelineName = pipelineName;
    }

    public void setDatasetVersion(String datasetVersion) { this.datasetVersion = datasetVersion;}
}
