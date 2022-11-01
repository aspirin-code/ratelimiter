{application,tocken_bucket,
             [{description,"requests limiter"},
              {vsn,"1"},
              {modules,[tocken_bucket,tocken_bucket_app,tocken_bucket_storage,
                        tocken_bucket_sup]},
              {registered,[tocken_bucket_sup,tocken_bucket_storage]},
              {applications,[kernel,stdlib]},
              {mod,{tocken_bucket_app,[]}}]}.
