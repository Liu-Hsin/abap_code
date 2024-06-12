### 使用完整的RESTFUL服务文档
1. 使用SE24创建一个类 ZCL_REST，并且继承超类 CL_REST_HTTP_HANDLER.
2. 重构方法 **IF_REST_APPLICATION~GET_ROOT_HANDLER**，**HANDLE_CSRF_TOKEN**，其中HANDLE_CSRF_TOKEN 是用于token验证的，如果不需要token验证则直接重构即可，不需要代码；
   需要验证的话可不重构，现有代码可用于验证token。
3. 重构代码如下：
   ![image](https://github.com/Liu-Hsin/abap_code/assets/57285504/f6103559-6d97-462a-b9f4-49d0fd73f758)
   其中 **/HttpService** 为接口路径，其他系统调用时需要按次路径来，**ZCL_RESTFUL** 为restful实施类。
   可添加多个不同的地址与类，用于不同的服务
4. TOKEN 验证，如果需要验证在使用GET方法时，在发送请求时需登录Authentication账号才可正确获取token，POST方法时带上即可。不需要token则直接重构即可，不需要代码。
5. 上面完成后再次使用SE24 创建一个类，类名为前面的实施类 **ZCL_RESTFUL** .并且继承超类 **CL_REST_RESOURCE**。
6. 重构你所需要的方法即可，如图：
   ![image](https://github.com/Liu-Hsin/abap_code/assets/57285504/c12bb9f1-6d9e-46dc-ba49-6c950c99142b)


