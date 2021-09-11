#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;
#[frame_support::pallet]
pub mod pallet {
	use frame_support::pallet_prelude::*;
	use frame_support::sp_runtime::traits::Hash;
	use frame_support::traits::Randomness;
	use frame_system::pallet_prelude::*;
	use sp_core::H256;

	#[pallet::pallet]
	#[pallet::generate_store(trait Store)]
	pub struct Pallet<T>(_);

	// EXPLA: to use T::Balance you need to add this: + pallet_balances::Config
	#[pallet::config]
	pub trait Config: frame_system::Config + pallet_balances::Config {
		type KittyRandomness: Randomness<H256, Self::BlockNumber>;
	}

	#[pallet::error]
    pub enum Error<T> {
         /// Nonce has overflowed past u64 limits
        NonceOverflow,
    }

	// ACTION (To-do at the end of this tutorial) : Write your storage item for `AllKittiesCount` here.
	// HINT: Always write #[pallet::storage] before you
	// declare any storage item.

	// Stores the total amount of Kitties in existence.
	#[pallet::storage]
	#[pallet::getter(fn get_kitties_count)]
	pub(super) type AllKittiesCount<T: Config> = StorageValue<_, u64, ValueQuery>;

	// An index to track of all Kitties.
	// index -> kitty hash
	#[pallet::storage]
	#[pallet::getter(fn kitty_by_index)]
	pub(super) type AllKittiesArray<T: Config> =
		StorageMap<_, Twox64Concat, u64, T::Hash, ValueQuery>;
	
	// Keeps track of all the Kitties.
	// kitty -> index
	#[pallet::storage]
	pub(super) type AllKittiesIndex<T: Config> =
		StorageMap<_, Twox64Concat, T::Hash, u64, ValueQuery>;

	#[pallet::storage]
    #[pallet::getter(fn get_nonce)]
    pub(super) type Nonce<T: Config> = StorageValue<_, u64, ValueQuery>;

	#[pallet::storage]
    #[pallet::getter(fn kitty)]
    pub(super) type Kitties<T: Config> =
        StorageMap<_, Twox64Concat, T::Hash, Kitty<T::Hash, T::Balance>, ValueQuery>;

	// Keeps track of what accounts own what Kitty.
	// Kitty Hash -> optional owner
	#[pallet::storage]
	#[pallet::getter(fn get_owner_of_kitty)]
	pub(super) type KittyOwner<T: Config> =
		StorageMap<_, Twox64Concat, T::Hash, T::AccountId, OptionQuery>;

    // Keep track of who a Kitty is owned by. (u64 is index)
	// (Owner, KittyIndex) -> Kitty Hash 
    #[pallet::storage]
    #[pallet::getter(fn get_kitty_of_owner_by_index)]
    pub(super) type OwnedKittiesArray<T: Config> =
        StorageMap<_, Twox64Concat, (T::AccountId, u64), T::Hash, ValueQuery>;

    // Keeps track of the total amount of Kitties owned.
	// owner -> amount of kitties
    #[pallet::storage]
    #[pallet::getter(fn get_owned_kitty_count)]
    pub(super) type OwnedKittiesCount<T: Config> =
        StorageMap<_, Twox64Concat, T::AccountId, u64, ValueQuery>;

	// Keeps track of all owned Kitties by index.
	// kitty hash -> index
    #[pallet::storage]
    pub(super) type OwnedKittiesIndex<T: Config> =
        StorageMap<_, Twox64Concat, T::Hash, u64, ValueQuery>;

	#[derive(Clone, Encode, Decode, Default, PartialEq)]
	pub struct Kitty<Hash, Balance> {
		/// a unique hash to identify each Kitty.
		id: Hash,
		/// the hash used to identify the DNA of a Kitty, which corresponds to its unique features. DNA is also used to breed new Kitties and to keep track of different Kitty generations
		dna: Hash,
		/// this is a balance that corresponds to the amount needed to buy a Kitty and determined by its owner
		price: Balance,
		/// an enum that can be either Male or Female
		gender: Gender,
	}

	#[derive(Encode, Decode, Debug, Clone, PartialEq)]
	pub enum Gender {
		Male,
		Female,
	}

	impl Default for Gender {
		fn default() -> Self {
			Gender::Male
		}
	}

	//** These are all our **//
	//** helper functions. **//

	impl<T: Config> Kitty<T, T> {
		pub fn gender(dna: T::Hash) -> Gender {
			if dna.as_ref()[0] % 2 == 0 {
				Gender::Male
			} else {
				Gender::Female
			}
		}
	}

	#[pallet::call]
	impl<T: Config> Pallet<T> {
	}

	// EXPLA: helper functions (often used functions)
	impl<T: Config> Pallet<T> {
		// EXPLA: use this nonce, because random_seed() does not change very often (within the block transactions would generate the same random seed)
		/// Generate a random hash, using the nonce as part of the hash
		fn random_hash(sender: &T::AccountId) -> T::Hash {
			let nonce = <Nonce<T>>::get();
			let seed = T::KittyRandomness::random_seed();
			T::Hashing::hash_of(&(seed, &sender, nonce))
		}

		/// increment nonce to generate a random hash for each tx inside a block
		fn increment_nonce() -> DispatchResult {
            <Nonce<T>>::try_mutate(|nonce| {
                let next = nonce.checked_add(1).ok_or(Error::<T>::NonceOverflow)?;
                *nonce = next;

                Ok(().into())
            })
        }
	}
}
