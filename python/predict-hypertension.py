import tensorflow as tf
import pandas as pd
import numpy as np

def perf_measure(y_actual, y_hat):
	TP = 0.0
	FP = 0.0
	TN = 0.0
	FN = 0.0
	for i in range(len(y_hat)): 
		if (y_hat[i]==1 and y_actual[i]==y_hat[i]): 
			TP += 1.0
		elif (y_hat[i]==1 and y_actual[i]!=y_hat[i]): 
			FP += 1.0
		elif (y_hat[i]==0 and y_actual[i]==y_hat[i]): 
			TN += 1.0
		elif (y_hat[i]==0 and y_actual[i]!=y_hat[i]): 
			FN += 1.0
	return(TP, FP, TN, FN)

# Dataset class
class DataSet(object):
	def __init__(self, inputs, labels):
		self._num_examples = inputs.shape[0]
		self._inputs = inputs
		self._labels = labels
		self._epochs_completed = 0
		self._index_in_epoch = 0
	@property
	def inputs(self):
		return self._inputs
	@property
	def labels(self):
		return self._labels
	@property
	def num_examples(self):
		return self._num_examples
	@property
	def epochs_completed(self):
		return self._epochs_completed
	def next_batch(self, batch_size):
		start = self._index_in_epoch
		self._index_in_epoch += batch_size
		if self._index_in_epoch > self._num_examples:
			end = self._num_examples
		else:
			end = self._index_in_epoch
			return self._inputs[start:end], self._labels[start:end]

def create_dataset(inputs, labels, fold, num_folds=5):
	class DataSets(object):
		pass
	db = DataSets()
	# No entities
	ndim = inputs.shape[0]
	# Splits
	test_size = int(round(ndim*100/num_folds/100))
	# Split data
	test_inputs = inputs[(fold-1)*test_size:(fold-1)*test_size+test_size]
	test_labels = labels[(fold-1)*test_size:(fold-1)*test_size+test_size]
	if fold == 1:
		train_inputs = inputs[test_size:]
		train_labels = labels[test_size:]
	elif fold == num_folds:
		train_inputs = inputs[:(fold-1)*test_size]
		train_labels = labels[:(fold-1)*test_size]
	else:
		train_inputs = pd.DataFrame(np.concatenate((inputs[:fold*test_size], inputs[(fold+1)*test_size:]),axis=0))
		train_labels = pd.DataFrame(np.concatenate((labels[:fold*test_size], labels[(fold+1)*test_size:]),axis=0))
	# set data
	db.train = DataSet(train_inputs, train_labels)
	db.test = DataSet(test_inputs, test_labels)
	return db


# Weight initialization with small noise for symmetry breaking and to prevent 0 gradients
def weight_variable(shape):
	initial = tf.truncated_normal(shape, stddev=0.1)
	return tf.Variable(initial)

# Bias initialization positive bias to avoid 'dead' neurons
def bias_variable(shape):
	initial = tf.constant(0.1, shape=shape)
	return tf.Variable(initial)

# Convolution uses a stride of one and are zero padded so that the output is the same size as the input
def conv2d(x, W):
	return tf.nn.conv2d(x, W, strides=[1, 1, 1, 1], padding='SAME')

# Pooling is old max pooling over 2x2 blocks
def max_pool_2x2(x):
	return tf.nn.max_pool(x, ksize=[1, 2, 2, 1],
	strides=[1, 2, 2, 1], padding='SAME')

# Two's compliment
def label_encoding(labels,label,num_classes=2):
	num_labels = labels.shape[0]
	labels_encoded = np.zeros((num_labels, num_classes))
	# print stats
	print("Chronic ({}): {}".format(label,labels[labels==1].count()))
	print("Class II (No {}): {}".format(label,labels[labels==0].count()))
	offsets = np.arange(num_labels) * num_classes
	bits = labels.astype(int)
	labels_encoded.flat[offsets + np.ravel(bits)] = 1
	return labels_encoded

# Initialize session
sess = tf.InteractiveSession()
# Variables
#input_labels = ["AGE","GENDER","FAMILY_SIZE","FAMILY_COMP_GRPD","OVERALL_SES","PLAN_MOVEMENT_KEY","LIFE","DDI","PSI","DMI","INCOME_BAND_KEY","UNDERWRITING_STATUS","ONCOLOGY_KEY","DURATION_INSURED","HVM","FUB","RUB","READMISSION_30_DAY_COUNT","EMERGENCY_VISIT_COUNT","IN_PATIENT_HOSP_COUNT","IN_PATIENT_HOSP_DAYS","CHRONIC_CONDITION_COUNT","DEG_CODE","PREV_DEG_CODE_1","AMOUNT_CLAIMED","IN_OUT_HOSPITAL","PROCEDURE_COUNT","BILLING_PRACTICE_TYPE","TOTAL_CLAIMS_IPM","TOTAL_CLAIMS_IPM3","TOTAL_CLAIMS_IPM6","TOTAL_CLAIMS_IPM12","TOTAL_CLAIM_AMOUNT_IPM","TOTAL_CLAIM_AMOUNT_IPM3","TOTAL_CLAIM_AMOUNT_IPM6","TOTAL_CLAIM_AMOUNT_IPM12","TOTAL_PROCS_IPM","TOTAL_PROCS_IPM3","TOTAL_PROCS_IPM6","TOTAL_PROCS_IPM12","WEIGHT","HEIGHT","WAIST","CHOLESTEROL","SYS_BP","DIA_BP","RANDOM_GLUCOSE","CIGS","DRINKS","FRUITVEG","DELTA_VA","PROP_OF_BENEFITS","HAS_GYM","GYM_FIRST_USE","DEDICATED_GYMMER","GYM_DANGER_ZONE","PHYSICAL_ACTIVITY_EVENTS_APM6","PHYSICAL_ACTIVITY_EVENTS_APM12","PHYSICAL_ACTIVITY_EVENTS_APM36","PHYSICAL_ACTIVITY_POINTS_APM6","PHYSICAL_ACTIVITY_POINTS_APM12","PHYSICAL_ACTIVITY_POINTS_APM36","PREFERRED_PARTNER","HF_GROUP","PARTNER_USAGE_SELECTION","UNHEALTHY_BEVERAGES_PER_MEMBER","CONFECTIONARY_PER_MEMBER","DAIRY_PER_MEMBER","PRE_PREPARED_MEALS_PER_MEMBER","FATS_OILS_PER_MEMBER","FRUIT_PER_MEMBER","VEGETABLES_PER_MEMBER","PROTEIN_PER_MEMBER","STARCHY_FOODS_PER_MEMBER","HEALTHY_FOOD_PER_MEMBER","UNHEALTHY_FOOD_PER_MEMBER","NEUTRAL_FOOD_PER_MEMBER","FAT_FREE_GOODS_PER_MEMBER","TOTAL_SPEND_PER_MEMBER","UNHEALTHY_BEVERAGES_COUNT_PER_MEMBER","CONFECTIONARY_COUNT_PER_MEMBER","DAIRY_COUNT_PER_MEMBER","PRE_PREPARED_MEALS_COUNT_PER_MEMBER","FATS_OILS_COUNT_PER_MEMBER","FRUIT_COUNT_PER_MEMBER","VEGETABLES_COUNT_PER_MEMBER","PROTEIN_COUNT_PER_MEMBER","STARCHY_FOODS_COUNT_PER_MEMBER","HEALTHY_FOOD_COUNT_PER_MEMBER","UNHEALTHY_FOOD_COUNT_PER_MEMBER","NEUTRAL_FOOD_COUNT_PER_MEMBER","FAT_FREE_GOODS_COUNT_PER_MEMBER","TOTAL_COUNT_PER_MEMBER","GYM_SPEND_APM3","GYM_SPEND_APM6","GYM_SPEND_APM12","TOTAL_TRNS_APM12","TOTAL_SPEND_SWIPES_APM12","DAYS_BETWEEN_TRNS_APM12","UTILIZATION_APM12","SIC_CODE_GROUPS_DISTINCT_APM12","FAST_FOOD_SPEND_APM3","FAST_FOOD_SPEND_APM6","FAST_FOOD_SPEND_APM12","FAST_FOOD_COUNT_APM3","FAST_FOOD_COUNT_APM6","FAST_FOOD_COUNT_APM12","DAYS_BETWEEN_FAST_FOOD","DAYS_BETWEEN_FAST_FOOD_APM12","HAS_MOVIE","SERIAL_MOVIEGOER","MOVIES_WATCHED_APM3","MOVIES_WATCHED_APM6","MOVIES_WATCHED_APM12","MOVIE_THEATER_SPEND_APM3","MOVIE_THEATER_SPEND_APM6","MOVIE_THEATER_SPEND_APM12","MOVIE_THEATER_COUNT_APM3","MOVIE_THEATER_COUNT_APM6","MOVIE_THEATER_COUNT_APM12","DAYS_BETWEEN_MOVIE_THEATER","DAYS_BETWEEN_MOVIE_THEATER_APM12","BAKERY_SPEND_SPEND_APM3","BAKERY_SPEND_SPEND_APM6","BAKERY_SPEND_SPEND_APM12","BAKERY_COUNT_APM3","BAKERY_COUNT_APM6","BAKERY_COUNT_APM12","DAYS_BETWEEN_BAKERY","DAYS_BETWEEN_BAKERY_APM12","GROCERIES_SPEND_APM3","GROCERIES_SPEND_APM6","GROCERIES_SPEND_APM12","GROCERIES_TRN_APM3","GROCERIES_TRN_APM6","GROCERIES_TRN_APM12","DAYS_BETWEEN_GROCERIES_APM12","TRAVEL_SPEND_APM12","TRAVEL_TRN_APM12","DAYS_BETWEEN_TRAVEL_APM12","MEDICAL_SPEND_APM3","MEDICAL_SPEND_APM6","MEDICAL_SPEND_APM12","MEDICAL_TRN_APM3","MEDICAL_TRN_APM6","MEDICAL_TRN_APM12","DAYS_BETWEEN_MEDICAL_APM12","BARS_AND_RESTAURANTS_SPEND_APM3","BARS_AND_RESTAURANTS_SPEND_APM6","BARS_AND_RESTAURANTS_SPEND_APM12","BARS_AND_RESTAURANTS_TRN_APM3","BARS_AND_RESTAURANTS_TRN_APM12","DAYS_BETWEEN_BARS_AND_RESTAURANTS_APM12","SPORTS_SPEND_APM3","SPORTS_SPEND_APM6","SPORTS_SPEND_APM12","SPORTS_TRN_APM3","SPORTS_TRN_APM6","SPORTS_TRN_APM12","DAYS_BETWEEN_SPORTS_APM12","FUEL_TRANSPORT_SPEND_APM3","FUEL_TRANSPORT_SPEND_APM6","FUEL_TRANSPORT_SPEND_APM12","FUEL_TRANSPORT_TRN_APM12","DAYS_BETWEEN_FUEL_APM12","HOME_IMPROVEMENT_SPEND_APM12","HOME_IMPROVEMENT_TRN_APM12","DAYS_BETWEEN_HOME_IMPROVEMENT_APM12","FASHION_SPEND_APM12","FASHION_TRN_APM12","DAYS_BETWEEN_CLOTHING_APM12","LIFESTYLE_SPENDER","ESAVVY_SPEND","ESAVVY_TRN_APM12","PHARMACY_SPEND_APM3","PHARMACY_SPEND_APM6","PHARMACY_SPEND_APM12","PHARMACY_COUNT_APM3","PHARMACY_COUNT_APM6","PHARMACY_COUNT_APM12","DAYS_BETWEEN_PHARMACY","DAYS_BETWEEN_PHARMACY_APM12"]
output_labels = ['Y_HYPERTENSION_DX_DATE_3YR','Y_DIABETES_DX_DATE_NEXT_3YR','Y_HYPERLIPIDEMIA_DX_DATE_3YR','Y_CAD_DX_DATE_3YR']
# Consts
predict_label = 0
batch_size = 50
nfolds = 5
dsplit = 20
# Input data
filepath = '/Users/thomas45/dev/tensorflow/predict.csv'
print 'Reading input data from '+filepath
print 'Predicting...%s'%output_labels[predict_label]
# Read dataset
data = pd.read_csv(filepath)
# Drop index
data.drop(data.columns[0],axis=1,inplace=True)
# Extract chronics
chronics = pd.DataFrame()
for i in range(len(output_labels)):
	chronics = chronics.append(data[data[output_labels[i]]==1],ignore_index=True)
	data.drop(data.index[data[output_labels[i]]==1],inplace=True)
# Extract chronics for prediction
predicts = chronics[chronics[output_labels[predict_label]]==1] 
npred = len(predicts)
# Shuffle the data
ndim = len(data)
perm = np.arange(ndim)
np.random.shuffle(perm)
data = data.iloc[perm]
# Create dataset accordint to split 
data = data.iloc[np.arange(npred*100/dsplit - npred)]
data = data.append(predicts,ignore_index=True)
# Shuffle the data
ndim = len(data)
perm = np.arange(ndim)
np.random.shuffle(perm)
data = data.iloc[perm]
# Extract inputs + labels
labels = label_encoding(data[output_labels[predict_label]],output_labels[predict_label])
inputs = data.drop(output_labels,axis=1).as_matrix()
print 'Total no. observations %d'%ndim
# FOlds
fACC = np.zeros(nfolds)
fTPR = np.zeros(nfolds)
fTNR = np.zeros(nfolds)
fPPV = np.zeros(nfolds)
fFS = np.zeros(nfolds)
for j in range(nfolds):
	print 'FOLD %d'%(j+1)
	# Create dataset
db = create_dataset(inputs,labels,j+1,nfolds)
# Dimension of inputs and outputs
xdim = db.train.inputs.shape[1]
ydim = db.train.labels.shape[1]
ndim = db.train.inputs.shape[0]
# Input data placeholder variable 
x = tf.placeholder('float', [None, xdim])
# Weights and biases
W = tf.Variable(tf.zeros([xdim,ydim]))
b = tf.Variable(tf.zeros([ydim]))
# Predictions
y_ = tf.placeholder('float', [None,ydim])
# First convolution layer - compute 32 features for each 4x4 patch
W_conv1 = weight_variable([4, 4, 1, 32])
b_conv1 = bias_variable([32])
# To apply convolution layer reshape input data
x_image = tf.reshape(x, [-1,16,16,1])
# Convolve input data with weight tensor and add bias and apply the RelU function 
h_conv1 = tf.nn.relu(conv2d(x_image, W_conv1) + b_conv1)
h_conv1.get_shape()
# Max pool
h_pool1 = max_pool_2x2(h_conv1)
h_pool1.get_shape()
# Second convolution layer - compute 64 features for every 4x4 patch
W_conv2 = weight_variable([4, 4, 32, 64])
b_conv2 = bias_variable([64])
# Convolve, add bias and apply RelU function 
h_conv2 = tf.nn.relu(conv2d(h_pool1, W_conv2) + b_conv2)
h_conv2.get_shape()
# Max pool
h_pool2 = max_pool_2x2(h_conv2)
h_pool2.get_shape()
# Add fully connected later with 1024 neurons to process entire image
W_fc1 = weight_variable([4 * 4 * 64, 1024])
b_fc1 = bias_variable([1024])
h_pool2_flat = tf.reshape(h_pool2, [-1, 4 * 4 * 64])
# Convolve, add bias and apply RelU function
h_fc1 = tf.nn.relu(tf.matmul(h_pool2_flat, W_fc1) + b_fc1)
# To reduce overfitting, apply dropout
keep_prob = tf.placeholder('float')
h_fc1_drop = tf.nn.dropout(h_fc1, keep_prob)
# Finally we add a SOFTMAX layer
W_fc2 = weight_variable([1024, ydim])
b_fc2 = bias_variable([ydim])
y_conv=tf.nn.softmax(tf.matmul(h_fc1_drop, W_fc2) + b_fc2)
# Train and evaluate model
cross_entropy = -tf.reduce_sum(y_*tf.log(y_conv))
train_step = tf.train.AdamOptimizer(1e-4).minimize(cross_entropy)
correct_prediction = tf.equal(tf.argmax(y_conv,1), tf.argmax(y_,1))
accuracy = tf.reduce_mean(tf.cast(correct_prediction, 'float'))
# Initialization
sess.run(tf.initialize_all_variables())
for i in range(ndim//batch_size):
	batch = db.train.next_batch(batch_size)
	if i%100 == 0:
		train_accuracy = accuracy.eval(feed_dict={x:batch[0], y_: batch[1], keep_prob: 1.0})
		print 'step %d, training accuracy %g'%(i, train_accuracy)
		train_step.run(feed_dict={x: batch[0], y_: batch[1], keep_prob: 0.5})
	print 'Assessing accuracy of predictions...'
# Accuracy
fACC[j] = accuracy.eval(feed_dict={x: db.test.inputs, y_: db.test.labels, keep_prob: 1.0})
print str(j+1)+"-Fold: Accuracy %g"%fACC[j]
y_hat = tf.argmax(y_conv,1).eval(feed_dict={x: db.test.inputs, y_: db.test.labels, keep_prob: 1.0})
y_actual = tf.argmax(y_,1).eval(feed_dict={x: db.test.inputs, y_: db.test.labels, keep_prob: 1.0})
	# Determine specificity and sensitivity
	(TP, FP, TN, FN) = perf_measure(y_actual,y_hat)
	# Sensitivity, hit rate, recall, or true positive rate
	fTPR[j] = TP/(TP+FN)
	print str(j+1)+'-Fold: Sensitivity %.4g'%fTPR[j]
	# Specificity or true negative rate
	fTNR[j] = TN/(TN+FP) 
	print str(j+1)+'-Fold: Specificity %.4g'%fTNR[j]
	# Precision or positive predictive value
	fPPV[j] = TP/(TP+FP)
	print str(j+1)+'-Fold: Prediction %.4g'%fPPV[j]
	# Score
	fFS[j] = 2*TP/(2*TP+FP+FN)
	print str(j+1)+'-Fold: F1 Score %.4g'%fPPV[j]
	
	

# Final results
print "Final accuracy %g"%np.mean(fACC)
print "Final sensitivity %g"%np.mean(fTPR)
print "Final specificity %g"%np.mean(fTNR)
print "Final predicivity %g"%np.mean(fPPV)
print "Final F1 score %g"%np.mean(fFS)
# Close session
sess.close()



