{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           App
import           Data.Binary                as Binary (Binary, byteSwap16,
                                                       decode, encode)
import qualified Data.ByteString.Lazy       as L
import           Data.DICOM                 as DICOM
import           Data.Word                  (Word16, Word8)
import           Monomer
import           System.Environment         as Env

import           Control.Monad.Trans.Except
import           Data.ByteString            as BS (ByteString)
import           Data.Text                  as Text (pack)
import           Types

class ByteSwappable a where
  byteSwap :: a -> a

instance ByteSwappable Word16 where
  byteSwap = byteSwap16

data DicomBaseInfo = DicomBaseInfo {
  dbiPixelData     :: ByteString,
  dbiRows          :: Word16,
  dbiColumns       :: Word16,
  dbiIntercept     :: Intercept,
  dbiSlope         :: Slope,
  dbiBitsAllocated :: BitsAllocated
} deriving (Eq, Show)


loadDicom :: FilePath -> ExceptT String IO DicomBaseInfo
loadDicom path = do
  dicomObj <- ExceptT $  DICOM.readObjectFromFile path
  let dicomMap = DICOM.toMap dicomObj
      eitherInfo = mapLeft show $ do
        pixBytes <- DICOM.pixelData dicomMap
        rows_ <- DICOM.rows dicomMap
        cols <- DICOM.columns dicomMap
        intercept <- DICOM.rescaleIntercept dicomMap
        slope <- DICOM.rescaleSlope dicomMap
        bitsAlloc <- DICOM.bitsAllocated dicomMap
        return DicomBaseInfo {
          dbiPixelData = pixBytes,
          dbiRows = rows_,
          dbiColumns = cols,
          dbiIntercept = intercept,
          dbiSlope = slope,
          dbiBitsAllocated = bitsAlloc
        }

  ExceptT (return eitherInfo)

loadDicomInteger :: FilePath -> ExceptT String IO DicomBaseInfo
loadDicomInteger path = do
  dicomObj <- ExceptT $  DICOM.readObjectFromFile path
  let dicomMap = DICOM.toMap dicomObj
      eitherInfo = mapLeft show $ do
        pixBytes <- DICOM.pixelData dicomMap
        rows_ <- DICOM.rows dicomMap
        cols <- DICOM.columns dicomMap
        bitsAlloc <- DICOM.bitsAllocated dicomMap
        return DicomBaseInfo {
          dbiPixelData = pixBytes,
          dbiRows = rows_,
          dbiColumns = cols,
          dbiIntercept = 0,
          dbiSlope = 0,
          dbiBitsAllocated = bitsAlloc
        }

  ExceptT (return eitherInfo)


loadDicoms :: FilePath -> FilePath -> ExceptT String IO (DicomBaseInfo, DicomBaseInfo)
loadDicoms path1 path2 = do
  dicom1 <- loadDicomInteger path1
  dicom2 <- loadDicomInteger path2
  return (dicom1, dicom2)

dicomPixelDataToList :: (Binary p) => Word16 -> Word16 -> ByteString -> [p]
dicomPixelDataToList rows_ columns_ pixelData_ =
  decode (L.append (encode ( (fromIntegral rows_* fromIntegral columns_) :: Int)) (L.fromStrict pixelData_))

dicomPixelDataToListBE  :: (Binary p, ByteSwappable p) => Word16 -> Word16 -> ByteString -> [p]
dicomPixelDataToListBE rows_ columns_ = map byteSwap  . dicomPixelDataToList rows_ columns_

main :: IO ()
main = do
  progName <- Env.getProgName
  args <- Env.getArgs

  case args of

    dicomPath1:dicomPath2:_ -> do

      dicoms <- runExceptT $  loadDicoms dicomPath1 dicomPath2


      case dicoms of
        Left err -> error err
        Right (dicom1, dicom2) ->
          let DicomBaseInfo {
                dbiPixelData=pixelData1,
                dbiRows=rows1,
                dbiColumns = columns1,
                dbiIntercept=intercept1,
                dbiSlope=slope1,
                dbiBitsAllocated=bitsAlloc1} = dicom1

              DicomBaseInfo {
                dbiPixelData=pixelData2,
                dbiRows=rows2,
                dbiColumns = columns2,
                dbiIntercept=intercept2,
                dbiSlope=slope2,
                dbiBitsAllocated=bitsAlloc2} = dicom2
          in

          if  intercept1 == 0 && intercept2 == 0
              && slope1 == 0 && slope2 == 0
              && bitsAlloc1 == 8 && bitsAlloc2 == 8 -- чи обидва зображення 8-бітні?

          then

            let dicomPixels1 :: [Word8] = dicomPixelDataToList rows1 columns1 pixelData1
                dicomPixels2 :: [Word8] = dicomPixelDataToList rows2 columns2 pixelData2

                model = AppModel {
                  appImg1 = SimpleImage {imgPixels=dicomPixels1, imgRows=fromIntegral rows1, imgColumns = fromIntegral columns1},
                  appImg2 = SimpleImage {imgPixels=dicomPixels2, imgRows=fromIntegral rows2, imgColumns = fromIntegral columns2},
                  appDisplayMode = DisplayFirst
                  }

                config = [
                  appWindowTitle (pack progName),
                  appWindowIcon "./assets/images/icon.png",
                  appWindowResizable False,
                  appTheme darkTheme,
                  appWindowState (MainWindowNormal (fromIntegral columns1, fromIntegral rows1)),
                  appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
                  appInitEvent AppInit
                  ]
            in startApp model handleEvent buildUI config

          else error "2 DICOMs are expected to be ubyte (Word8)"

    _ -> error "Pass 2 arguments: dicom1 path and dicom2 path"
