FROM dukegcb/openshift-shiny-verse:4.1.2

RUN install2.r plotly
RUN install2.r vipor

ADD ./src /srv/code
