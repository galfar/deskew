unit ImagingWic;

interface

implementation

uses
  Windows, ActiveX;

const
  SID_IWICPalette                         = '{00000040-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmapSource                    = '{00000120-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICFormatConverter                 = '{00000301-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmapScaler                    = '{00000302-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmapClipper                   = '{E4FBCF03-223D-4e81-9333-D635556DD1B5}';
  SID_IWICBitmapFlipRotator               = '{5009834F-2D6A-41ce-9E1B-17C5AFF7A782}';
  SID_IWICBitmapLock                      = '{00000123-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmap                          = '{00000121-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICColorTransform                  = '{B66F034F-D0E2-40ab-B436-6DE39E321A94}';
  SID_IWICColorContext                    = '{3C613A02-34B2-44ea-9A7C-45AEA9C6FD6D}';
  SID_IWICFastMetadataEncoder             = '{B84E2C09-78C9-4AC4-8BD3-524AE1663A2F}';
  SID_IWICStream                          = '{135FF860-22B7-4ddf-B0F6-218F4F299A43}';
  SID_IWICEnumMetadataItem                = '{DC2BB46D-3F07-481E-8625-220C4AEDBB33}';
  SID_IWICMetadataQueryReader             = '{30989668-E1C9-4597-B395-458EEDB808DF}';
  SID_IWICMetadataQueryWriter             = '{A721791A-0DEF-4d06-BD91-2118BF1DB10B}';
  SID_IWICBitmapEncoder                   = '{00000103-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmapFrameEncode               = '{00000105-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmapDecoder                   = '{9EDDE9E7-8DEE-47ea-99DF-E6FAF2ED44BF}';
  SID_IWICBitmapSourceTransform           = '{3B16811B-6A43-4ec9-B713-3D5A0C13B940}';
  SID_IWICBitmapFrameDecode               = '{3B16811B-6A43-4ec9-A813-3D930C13B940}';
  SID_IWICProgressiveLevelControl         = '{DAAC296F-7AA5-4dbf-8D15-225C5976F891}';
  SID_IWICProgressCallback                = '{4776F9CD-9517-45FA-BF24-E89C5EC5C60C}';
  SID_IWICBitmapCodecProgressNotification = '{64C1024E-C3CF-4462-8078-88C2B11C46D9}';
  SID_IWICComponentInfo                   = '{23BC3F0A-698B-4357-886B-F24D50671334}';
  SID_IWICFormatConverterInfo             = '{9F34FB65-13F4-4f15-BC57-3726B5E53D9F}';
  SID_IWICBitmapCodecInfo                 = '{E87A44C4-B76E-4c47-8B09-298EB12A2714}';
  SID_IWICBitmapEncoderInfo               = '{94C9B4EE-A09F-4f92-8A1E-4A9BCE7E76FB}';
  SID_IWICBitmapDecoderInfo               = '{D8CD007F-D08F-4191-9BFC-236EA7F0E4B5}';
  SID_IWICPixelFormatInfo                 = '{E8EDA601-3D48-431a-AB44-69059BE88BBE}';
  SID_IWICPixelFormatInfo2                = '{A9DB33A2-AF5F-43C7-B679-74F5984B5AA4}';
  SID_IWICImagingFactory                  = '{ec5ec8a9-c395-4314-9c77-54d7a935ff70}';
  SID_IWICDevelopRawNotificationCallback  = '{95c75a6e-3e8c-4ec2-85a8-aebcc551e59b}';
  SID_IWICDevelopRaw                      = '{fbec5e44-f7be-4b65-b7f8-c0c81fef026d}';

  CLSID_WICImagingFactory:           TGUID = '{CACAF262-9370-4615-A13B-9F5539DA4C0A}';
  GUID_VendorMicrosoft:              TGUID = '{F0E749CA-EDEF-4589-A73A-EE0E626A2A2B}';
  GUID_VendorMicrosoftBuiltIn:       TGUID = '{257A30FD-06B6-462B-AEA4-63F70B86E533}';
  CLSID_WICBmpDecoder:               TGUID = '{6B462062-7CBF-400D-9FDB-813DD10F2778}';
  CLSID_WICPngDecoder:               TGUID = '{389EA17B-5078-4CDE-B6EF-25C15175C751}';
  CLSID_WICIcoDecoder:               TGUID = '{C61BFCDF-2E0F-4AAD-A8D7-E06BAFEBCDFE}';
  CLSID_WICJpegDecoder:              TGUID = '{9456A480-E88B-43EA-9E73-0B2D9B71B1CA}';
  CLSID_WICGifDecoder:               TGUID = '{381DDA3C-9CE9-4834-A23E-1F98F8FC52BE}';
  CLSID_WICTiffDecoder:              TGUID = '{B54E85D9-FE23-499F-8B88-6ACEA713752B}';
  CLSID_WICWmpDecoder:               TGUID = '{A26CEC36-234C-4950-AE16-E34AACE71D0D}';
  CLSID_WICBmpEncoder:               TGUID = '{69BE8BB4-D66D-47C8-865A-ED1589433782}';
  CLSID_WICPngEncoder:               TGUID = '{27949969-876A-41D7-9447-568F6A35A4DC}';
  CLSID_WICJpegEncoder:              TGUID = '{1A34F5C1-4A5A-46DC-B644-1F4567E7A676}';
  CLSID_WICGifEncoder:               TGUID = '{114F5598-0B22-40A0-86A1-C83EA495ADBD}';
  CLSID_WICTiffEncoder:              TGUID = '{0131BE10-2001-4C5F-A9B0-CC88FAB64CE8}';
  CLSID_WICWmpEncoder:               TGUID = '{AC4CE3CB-E1C1-44CD-8215-5A1665509EC2}';
  GUID_ContainerFormatBmp:           TGUID = '{0AF1D87E-FCFE-4188-BDEB-A7906471CBE3}';
  GUID_ContainerFormatPng:           TGUID = '{1B7CFAF4-713F-473C-BBCD-6137425FAEAF}';
  GUID_ContainerFormatIco:           TGUID = '{A3A860C4-338F-4C17-919A-FBA4B5628F21}';
  GUID_ContainerFormatJpeg:          TGUID = '{19E4A5AA-5662-4FC5-A0C0-1758028E1057}';
  GUID_ContainerFormatTiff:          TGUID = '{163BCC30-E2E9-4F0B-961D-A3E9FDB788A3}';
  GUID_ContainerFormatGif:           TGUID = '{1F8A5601-7D4D-4CBD-9C82-1BC8D4EEB9A5}';
  GUID_ContainerFormatWmp:           TGUID = '{57A37CAA-367A-4540-916B-F183C5093A4B}';
  CLSID_WICImagingCategories:        TGUID = '{FAE3D380-FEA4-4623-8C75-C6B61110B681}';
  CATID_WICBitmapDecoders:           TGUID = '{7ED96837-96F0-4812-B211-F13C24117ED3}';
  CATID_WICBitmapEncoders:           TGUID = '{AC757296-3522-4E11-9862-C17BE5A1767E}';
  CATID_WICPixelFormats:             TGUID = '{2B46E70F-CDA7-473E-89F6-DC9630A2390B}';
  CATID_WICFormatConverters:         TGUID = '{7835EAE8-BF14-49D1-93CE-533A407B2248}';
  CATID_WICMetadataReader:           TGUID = '{05AF94D8-7174-4CD2-BE4A-4124B80EE4B8}';
  CATID_WICMetadataWriter:           TGUID = '{ABE3B9A4-257D-4B97-BD1A-294AF496222E}';
  CLSID_WICDefaultFormatConverter:   TGUID = '{1A3F11DC-B514-4B17-8C5F-2154513852F1}';
  CLSID_WICFormatConverterHighColor: TGUID = '{AC75D454-9F37-48F8-B972-4E19BC856011}';
  CLSID_WICFormatConverterNChannel:  TGUID = '{C17CABB2-D4A3-47D7-A557-339B2EFBD4F1}';
  CLSID_WICFormatConverterWMPhoto:   TGUID = '{9CB5172B-D600-46BA-AB77-77BB7E3A00D9}';

  GUID_WICPixelFormatUndefined:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC900}';
  GUID_WICPixelFormatDontCare:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC900}';
  GUID_WICPixelFormat1bppIndexed:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC901}';
  GUID_WICPixelFormat2bppIndexed:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC902}';
  GUID_WICPixelFormat4bppIndexed:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC903}';
  GUID_WICPixelFormat8bppIndexed:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC904}';
  GUID_WICPixelFormatBlackWhite:           TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC905}';
  GUID_WICPixelFormat2bppGray:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC906}';
  GUID_WICPixelFormat4bppGray:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC907}';
  GUID_WICPixelFormat8bppGray:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC908}';
  GUID_WICPixelFormat8bppAlpha:            TGUID = '{E6CD0116-EEBA-4161-AA85-27DD9FB3A895}';
  GUID_WICPixelFormat16bppBGR555:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC909}';
  GUID_WICPixelFormat16bppBGR565:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90A}';
  GUID_WICPixelFormat16bppBGRA5551:        TGUID = '{05EC7C2B-F1E6-4961-AD46-E1CC810A87D2}';
  GUID_WICPixelFormat16bppGray:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90B}';
  GUID_WICPixelFormat24bppBGR:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90C}';
  GUID_WICPixelFormat24bppRGB:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90D}';
  GUID_WICPixelFormat32bppBGR:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90E}';
  GUID_WICPixelFormat32bppBGRA:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90F}';
  GUID_WICPixelFormat32bppPBGRA:           TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC910}';
  GUID_WICPixelFormat32bppGrayFloat:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC911}';
  GUID_WICPixelFormat32bppRGBA:            TGUID = '{F5C7AD2D-6A8D-43DD-A7A8-A29935261AE9}';
  GUID_WICPixelFormat32bppPRGBA:           TGUID = '{3CC4A650-A527-4D37-A916-3142C7EBEDBA}';
  GUID_WICPixelFormat48bppRGB:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC915}';
  GUID_WICPixelFormat48bppBGR:             TGUID = '{E605A384-B468-46CE-BB2E-36F180E64313}';
  GUID_WICPixelFormat64bppRGBA:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC916}';
  GUID_WICPixelFormat64bppBGRA:            TGUID = '{1562FF7C-D352-46F9-979E-42976B792246}';
  GUID_WICPixelFormat64bppPRGBA:           TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC917}';
  GUID_WICPixelFormat64bppPBGRA:           TGUID = '{8C518E8E-A4EC-468B-AE70-C9A35A9C5530}';
  GUID_WICPixelFormat16bppGrayFixedPoint:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC913}';
  GUID_WICPixelFormat32bppBGR101010:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC914}';
  GUID_WICPixelFormat48bppRGBFixedPoint:   TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC912}';
  GUID_WICPixelFormat48bppBGRFixedPoint:   TGUID = '{49CA140E-CAB6-493B-9DDF-60187C37532A}';
  GUID_WICPixelFormat96bppRGBFixedPoint:   TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC918}';
  GUID_WICPixelFormat128bppRGBAFloat:      TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC919}';
  GUID_WICPixelFormat128bppPRGBAFloat:     TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91A}';
  GUID_WICPixelFormat128bppRGBFloat:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91B}';
  GUID_WICPixelFormat32bppCMYK:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91C}';
  GUID_WICPixelFormat64bppRGBAFixedPoint:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91D}';
  GUID_WICPixelFormat64bppBGRAFixedPoint:  TGUID = '{356de33c-54d2-4a23-bb04-9b7bf9b1d42d}';
  GUID_WICPixelFormat64bppRGBFixedPoint:   TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC940}';
  GUID_WICPixelFormat128bppRGBAFixedPoint: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91E}';
  GUID_WICPixelFormat128bppRGBFixedPoint:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC941}';
  GUID_WICPixelFormat64bppRGBAHalf:        TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93A}';
  GUID_WICPixelFormat64bppRGBHalf:         TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC942}';
  GUID_WICPixelFormat48bppRGBHalf:         TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93B}';
  GUID_WICPixelFormat32bppRGBE:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93D}';
  GUID_WICPixelFormat16bppGrayHalf:        TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93E}';
  GUID_WICPixelFormat32bppGrayFixedPoint:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93F}';
  GUID_WICPixelFormat32bppRGBA1010102:     TGUID = '{25238D72-FCF9-4522-B514-5578E5AD55E0}';
  GUID_WICPixelFormat32bppRGBA1010102XR:   TGUID = '{00DE6B9A-C101-434B-B502-D0165EE1122C}';
  GUID_WICPixelFormat64bppCMYK:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91F}';
  GUID_WICPixelFormat24bpp3Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC920}';
  GUID_WICPixelFormat32bpp4Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC921}';
  GUID_WICPixelFormat40bpp5Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC922}';
  GUID_WICPixelFormat48bpp6Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC923}';
  GUID_WICPixelFormat56bpp7Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC924}';
  GUID_WICPixelFormat64bpp8Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC925}';
  GUID_WICPixelFormat48bpp3Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC926}';
  GUID_WICPixelFormat64bpp4Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC927}';
  GUID_WICPixelFormat80bpp5Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC928}';
  GUID_WICPixelFormat96bpp6Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC929}';
  GUID_WICPixelFormat112bpp7Channels:      TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92A}';
  GUID_WICPixelFormat128bpp8Channels:      TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92B}';
  GUID_WICPixelFormat40bppCMYKAlpha:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92C}';
  GUID_WICPixelFormat80bppCMYKAlpha:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92D}';
  GUID_WICPixelFormat32bpp3ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92E}';
  GUID_WICPixelFormat40bpp4ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92F}';
  GUID_WICPixelFormat48bpp5ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC930}';
  GUID_WICPixelFormat56bpp6ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC931}';
  GUID_WICPixelFormat64bpp7ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC932}';
  GUID_WICPixelFormat72bpp8ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC933}';
  GUID_WICPixelFormat64bpp3ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC934}';
  GUID_WICPixelFormat80bpp4ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC935}';
  GUID_WICPixelFormat96bpp5ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC936}';
  GUID_WICPixelFormat112bpp6ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC937}';
  GUID_WICPixelFormat128bpp7ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC938}';
  GUID_WICPixelFormat144bpp8ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC939}';

type
  IWICStream = interface(IStream)
    [SID_IWICStream]
    function InitializeFromIStream(pIStream: IStream): HRESULT; stdcall;
    function InitializeFromFilename(wzFileName: LPCWSTR;
      dwDesiredAccess: DWORD): HRESULT; stdcall;
    function InitializeFromMemory(pbBuffer: WICInProcPointer;
      cbBufferSize: DWORD): HRESULT; stdcall;
    function InitializeFromIStreamRegion(pIStream: IStream;
      ulOffset: ULARGE_INTEGER; ulMaxSize: ULARGE_INTEGER): HRESULT; stdcall;
  end;


class function TCanvasD2D.ImagingFactory: IWICImagingFactory;
begin
  if not Assigned(FImagingFactory) then
  begin
    CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,
      IUnknown, FImagingFactory);
  end;
  Result := FImagingFactory;
end;


end.
