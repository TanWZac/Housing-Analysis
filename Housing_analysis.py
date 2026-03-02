"""Python implementation of the housing price analysis pipeline.

This script mirrors and extends the existing R workflow with:
1) Random Forest regression (with grid-search tuning)
2) XGBoost regression (with one-hot encoded categorical features + grid-search tuning)
3) Polynomial regression using `area`

It reports evaluation metrics for each model:
- MAE
- MSE
- RMSE
- R²
"""

from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import GridSearchCV, train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, PolynomialFeatures
from xgboost import XGBRegressor

RANDOM_STATE = 42
PLOT_DIR = Path("plots")


def evaluate_model(model_name: str, y_true: pd.Series, y_pred: pd.Series) -> dict:
    """Return a dictionary of standard regression metrics."""
    mse = mean_squared_error(y_true, y_pred)
    return {
        "Model": model_name,
        "MAE": mean_absolute_error(y_true, y_pred),
        "MSE": mse,
        "RMSE": mse ** 0.5,
        "R2": r2_score(y_true, y_pred),
    }


def plot_actual_vs_predicted(y_true: pd.Series, y_pred: pd.Series, title: str, file_name: str) -> None:
    """Scatter plot with regression trend line for actual vs predicted prices."""
    plt.figure(figsize=(7, 5))
    sns.scatterplot(x=y_true, y=y_pred, alpha=0.7)
    sns.regplot(x=y_true, y=y_pred, scatter=False, color="red", ci=None)
    plt.xlabel("Actual Price")
    plt.ylabel("Predicted Price")
    plt.title(title)
    plt.tight_layout()
    plt.savefig(PLOT_DIR / file_name, dpi=150)
    plt.close()


def plot_heatmap(y_true: pd.Series, y_pred: pd.Series, title: str, file_name: str) -> None:
    """2D histogram heatmap for actual vs predicted prices."""
    plt.figure(figsize=(7, 5))
    plt.hist2d(y_true, y_pred, bins=20)
    plt.colorbar(label="Count")
    plt.xlabel("Actual Price")
    plt.ylabel("Predicted Price")
    plt.title(title)
    plt.tight_layout()
    plt.savefig(PLOT_DIR / file_name, dpi=150)
    plt.close()


def main() -> None:
    PLOT_DIR.mkdir(exist_ok=True)

    data = pd.read_csv("Housing.csv")

    # Shared split for RF and polynomial models.
    train_data, test_data = train_test_split(data, test_size=0.2, random_state=RANDOM_STATE)

    # -------------------------------
    # 1) Random Forest (grid searched)
    # -------------------------------
    X_train_rf = train_data.drop(columns=["price"])
    y_train_rf = train_data["price"]
    X_test_rf = test_data.drop(columns=["price"])
    y_test_rf = test_data["price"]

    categorical_cols = X_train_rf.select_dtypes(include=["object", "category"]).columns.tolist()
    numeric_cols = X_train_rf.select_dtypes(exclude=["object", "category"]).columns.tolist()

    rf_preprocessor = ColumnTransformer(
        transformers=[
            ("cat", OneHotEncoder(handle_unknown="ignore"), categorical_cols),
            ("num", "passthrough", numeric_cols),
        ]
    )

    rf_pipeline = Pipeline(
        steps=[
            ("preprocessor", rf_preprocessor),
            ("model", RandomForestRegressor(random_state=RANDOM_STATE)),
        ]
    )

    rf_param_grid = {
        "model__n_estimators": [200, 400],
        "model__max_depth": [None, 10, 20],
        "model__min_samples_split": [2, 5],
        "model__min_samples_leaf": [1, 2],
    }

    rf_search = GridSearchCV(
        estimator=rf_pipeline,
        param_grid=rf_param_grid,
        scoring="neg_root_mean_squared_error",
        cv=5,
        n_jobs=-1,
        verbose=0,
    )
    rf_search.fit(X_train_rf, y_train_rf)
    rf_predictions = rf_search.best_estimator_.predict(X_test_rf)

    rf_metrics = evaluate_model("RandomForest (GridSearchCV)", y_test_rf, rf_predictions)

    plot_actual_vs_predicted(
        y_test_rf,
        rf_predictions,
        "Random Forest (Tuned): Actual vs Predicted Prices",
        "random_forest_actual_vs_predicted.png",
    )
    plot_heatmap(
        y_test_rf,
        rf_predictions,
        "Random Forest (Tuned): Heatmap of Actual vs Predicted",
        "random_forest_heatmap.png",
    )

    # ------------------------------------------
    # 2) XGBoost (grid searched)
    # ------------------------------------------
    X = data.drop(columns=["price"])
    y = data["price"]

    X_train_xgb_df, X_test_xgb_df, y_train_xgb, y_test_xgb = train_test_split(
        X, y, test_size=0.3, random_state=RANDOM_STATE
    )

    cat_cols_xgb = X_train_xgb_df.select_dtypes(include=["object", "category"]).columns.tolist()
    num_cols_xgb = X_train_xgb_df.select_dtypes(exclude=["object", "category"]).columns.tolist()

    xgb_pipeline = Pipeline(
        steps=[
            (
                "preprocessor",
                ColumnTransformer(
                    transformers=[
                        ("cat", OneHotEncoder(handle_unknown="ignore"), cat_cols_xgb),
                        ("num", "passthrough", num_cols_xgb),
                    ]
                ),
            ),
            (
                "model",
                XGBRegressor(
                    objective="reg:squarederror",
                    random_state=RANDOM_STATE,
                    n_jobs=-1,
                ),
            ),
        ]
    )

    xgb_param_grid = {
        "model__n_estimators": [100, 250],
        "model__max_depth": [4, 6],
        "model__learning_rate": [0.05, 0.1],
        "model__subsample": [0.8, 1.0],
        "model__colsample_bytree": [0.8, 1.0],
    }

    xgb_search = GridSearchCV(
        estimator=xgb_pipeline,
        param_grid=xgb_param_grid,
        scoring="neg_root_mean_squared_error",
        cv=3,
        n_jobs=-1,
        verbose=0,
    )
    xgb_search.fit(X_train_xgb_df, y_train_xgb)
    xgb_predictions = xgb_search.best_estimator_.predict(X_test_xgb_df)

    xgb_metrics = evaluate_model("XGBoost (GridSearchCV)", y_test_xgb, xgb_predictions)
    plot_actual_vs_predicted(
        y_test_xgb,
        xgb_predictions,
        "XGBoost (Tuned): Actual vs Predicted Prices",
        "xgboost_actual_vs_predicted.png",
    )

    # ----------------------------------
    # 3) Polynomial Regression on `area`
    # ----------------------------------
    X_train_poly = train_data[["area"]]
    y_train_poly = train_data["price"]
    X_test_poly = test_data[["area"]]
    y_test_poly = test_data["price"]

    poly_model = Pipeline(
        steps=[
            ("poly", PolynomialFeatures(degree=2, include_bias=False)),
            ("linear", LinearRegression()),
        ]
    )
    poly_model.fit(X_train_poly, y_train_poly)
    poly_predictions = poly_model.predict(X_test_poly)
    poly_metrics = evaluate_model("PolynomialRegression(area^2)", y_test_poly, poly_predictions)

    poly_plot_df = pd.DataFrame(
        {
            "area": X_test_poly["area"],
            "actual": y_test_poly,
            "predicted": poly_predictions,
        }
    ).sort_values("area")

    plt.figure(figsize=(7, 5))
    sns.scatterplot(data=poly_plot_df, x="area", y="actual", label="Actual", alpha=0.7)
    plt.plot(poly_plot_df["area"], poly_plot_df["predicted"], color="red", label="Predicted")
    plt.title("Polynomial Regression: Actual vs Predicted Prices")
    plt.xlabel("Area")
    plt.ylabel("Price")
    plt.legend()
    plt.tight_layout()
    plt.savefig(PLOT_DIR / "polynomial_regression_area.png", dpi=150)
    plt.close()

    # Consolidated metrics and best-parameter outputs.
    metrics_table = pd.DataFrame([rf_metrics, xgb_metrics, poly_metrics]).round(4)
    metrics_table.to_csv("model_evaluation_metrics.csv", index=False)

    best_params = pd.DataFrame(
        [
            {
                "Model": "RandomForest",
                "BestParams": rf_search.best_params_,
                "BestCVScore_RMSE": -rf_search.best_score_,
            },
            {
                "Model": "XGBoost",
                "BestParams": xgb_search.best_params_,
                "BestCVScore_RMSE": -xgb_search.best_score_,
            },
        ]
    )
    best_params.to_csv("model_best_params.csv", index=False)

    print("\nModel Evaluation Metrics")
    print(metrics_table.to_string(index=False))
    print("\nBest Hyperparameters")
    print(best_params.to_string(index=False))
    print("\nSaved metrics: model_evaluation_metrics.csv")
    print("Saved best params: model_best_params.csv")
    print(f"Saved plots in: {PLOT_DIR.resolve()}")


if __name__ == "__main__":
    main()
