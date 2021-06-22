## COMPAS(LH)주관 "세종특별자치시 주택시장 분석"
 > 세부적인 시각화 자료는 pdf파일에 함께 첨부하였습니다.

### 주요내용
 - Part1. 세종시 동향 파악
  > 1) 인구 및 가구현황에 따른 주택수요 현황 파악
  > 2) 주택공급 현황 파악
  > 3) 자가율 현황에 따른 자가 및 차가 선택 현황 파악
  > 4) 보유율 현황에 따른 보유형태 선택 현황 파악
  > 5) 분양권 이슈에 따른 최근 정책적 이슈의 영향 파악
  > 6) 전출입 현황에 따른 세종시 내외로의 주거이동 동향 파악
  > 7) 세종시 수요권에 따른 동일 수요권역 파악
  
 - Part2. 가격 및 거래동향 파악
  > 1) 반복매매지수를 활용하여 매매/전세/월세 가격수준 파악
  > 2) 연도별 매매/전세/거래량 변화 파악
  > 3) 최고증가 시점의 읍면동별 가격 수준 파악
  > 4) 전월세 간 가격수준의 변화 파악
  
### 관련된 이론
1. 수요권 산정: 수용가능주택 1호 선택확률(전출률 이용)을 토대로 수요권을 산정 
 > 주거이동이란 실질적으로 전입 행정구역 내의 집 한채를 선택하는 형태이기 때문에 전출률을 이용하여 산정하며 재고주택과 신규주택 중 하나를 선택하는 구도이므로 재고주택과 신규주택을 이용해 수용가능주택을 산정한다. 
 > 이를 통해 최종적으로 전입구역 내의 수용가능한 주택 1채를 선택하여 주거이동할 확률을 산정 (이창무·한제선·정상준,2019) 
 
2. 반복매매지수: 두 번 이상 거래가 된 주택들(반복매매)을 대상으로 주택가격지수를 산정하는 방법론
 > 1) 본 분석에서는 현재 국토교통부에서 사용하는 (Bailey et., 1963) 동일가중평균방식의 기하평균 반복매매지수(OLS)를 사용함
 > 2) 동일주택들을 대상으로 매매가 이루어진 기간 중 주택의 특성은 고정되어 있는 것으로 가정하며 기본 산식은 아래와 같음
 
 
 > 3) 거래량이 충분히 확보되지 않으면 지수산정에 어려움이 있기 때문에, 모든 개별주택의 거래가 지수산정에 충분한 양이 확보되도록 동일주택의 범위를 설정 (ex. 아파트의 경우 읍면동+본번+부번+층그룹+면적 / 단독다가구의 경우 읍면동으로 설정)
 > 4) 종속변수에 사용되는 매매가격의 경우, 주택가격자료의 이분산성을 고려하여 단위면적당 가격을 이용하여 변동률을 산정함 :
아파트는 거래가격에 전용면적을 나누어 산정, 단독다가구는 거래가격에 토지면적을 나누어 산정(류강민·최성호·이상영, 2012)
 > 5) 최종적으로 OLS회귀분석 결과에 exp을 취해주어 각 시점의 지수를 산정(종속변수가 로그화되어있기 때문)

  
  
  
### 사용언어
 > 1) 분석: python, r
 > 2) 시각화패키지: plotly, folium

### 사용한 데이터
 > 1) COMPAS 제공 자체 데이터
 > 2) 주거실태조사 데이터
 > 3) 한국부동산원
 > 4) 국내인구이동통계
 
 
