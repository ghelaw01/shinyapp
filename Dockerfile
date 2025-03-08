FROM r-base
RUN R -e "install.packages(c('plumber', 'xgboost', 'data.table', 'jsonlite', 'caret'))"
WORKDIR /app
COPY . /app
CMD ["Rscript", "app.R"]